package xiangshan.backend.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._

//MatrixTE
//该模块的设计目标是，外积模块，组织多个ReducePE，来复用矩阵乘的数据，MatrixTE接受ABScarchPad的数据，交给broadcaster，生成数据馈送至至ReducePE，将Reduce的输出数据准备馈送至ScarchPadC。
//计算上来看，它的输入是两个向量，将两个向量广播成两个相同大小的矩阵后，将元素送入ReducuPE。向量的大小也很明显其一是Matrix_M，其二是Matrix_N，向量内的元素宽度是Reduce_Width
class MatrixTE(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new Bundle{
    val VectorA = Flipped(DecoupledIO(UInt((ReduceWidth*Matrix_M).W)))
    val VectorB = Flipped(DecoupledIO(UInt((ReduceWidth*Matrix_N).W)))
    val MatirxC = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W)))
    val MatrixD = DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W))
    val ConfigInfo = Flipped(DecoupledIO(new ConfigInfoIO))
    val ComputeGo            = Output(Bool())
  })

  //实例化ReducePE
  val Matrix = VecInit.tabulate(Matrix_M, Matrix_N){(x,y) => Module(new ReducePE(x*4+y)).io}

  //直接驱动ReducePE的输入
  //接下来给每个ReducePE的输入进行赋值
  //broadcaster的逻辑
  for (i <- 0 until Matrix_M){
    for (j <- 0 until Matrix_N){
      Matrix(i)(j).ReduceA.bits       := io.VectorA.bits((i+1)*ReduceWidth-1,(i)*ReduceWidth)
      Matrix(i)(j).ReduceA.valid      := io.VectorA.valid
      Matrix(i)(j).ReduceB.bits       := io.VectorB.bits((j+1)*ReduceWidth-1,(j)*ReduceWidth)
      Matrix(i)(j).ReduceB.valid      := io.VectorB.valid
      Matrix(i)(j).AddC.bits          := io.MatirxC.bits((i*Matrix_N+j+1)*ResultWidth-1,(i*Matrix_N+j)*ResultWidth)
      Matrix(i)(j).AddC.valid         := io.MatirxC.valid
      Matrix(i)(j).ConfigInfo.bits    := io.ConfigInfo.bits
      Matrix(i)(j).ConfigInfo.valid   := io.ConfigInfo.valid
      when(io.VectorA.valid && io.VectorB.valid && io.MatirxC.valid){
        // printf("[MatrixTE]: Matrix(%d)(%d) ReduceA:%x ReduceB:%x AddC:%x\n",i.U,j.U,Matrix(i)(j).ReduceA.bits,Matrix(i)(j).ReduceB.bits,Matrix(i)(j).AddC.bits)
      }
    }
  }

  //将每个ReducePE的输出拼接成一个矩阵，然后送入MatrixD
  val CurrentMatrixD = Wire(Vec(Matrix_M*Matrix_N, UInt(ResultWidth.W)))
  for (i <- 0 until Matrix_M){
    for (j <- 0 until Matrix_N){
      CurrentMatrixD(i*Matrix_N+j) := Matrix(i)(j).ResultD.bits
    }
  }
  //这里asUInt可以将Vec(UInt)转换成UInt
  io.MatrixD.bits := CurrentMatrixD.asUInt
  io.MatrixD.valid := Matrix(0)(0).ResultD.valid
  //如果Matrix(i)(j)的每个vali都为true，那么MatrixD的valid才为true
  // val YJP_valid_count = RegInit(0.U(2.W))
  // val YJP_Count_number = RegInit(0.U(32.W))
  // when(io.VectorA.valid && io.VectorB.valid){
  //     YJP_valid_count := YJP_valid_count + 1.U
  //     YJP_Count_number := YJP_Count_number + 1.U
  //     when(YJP_valid_count === 3.U){
  //         io.MatrixD.bits := YJP_Count_number
  //         io.MatrixD.valid := true.B
  //     }.otherwise{
  //         io.MatrixD.valid := false.B
  //     }
  // }
  // io.MatrixD.valid := Matrix.map(_.map(_.ResultD.valid).reduce(_&&_)).reduce(_&&_)

  //TODO:注意，这里的ready可以不用这么判断，我们所有PE是锁步的，能保证所有PE的数据同时到达，所以只要考察一个PE即可
  //确定所有的ready信号
  //当所有的ReducePE的输入都ready的时候，VectorA和VectorB的ready才为true
  //注意这里如果是时序不足的点，很简单只用考察一个PE即可，因为所有PE是同步执行的，这里这样写是保证逻辑完整完备，代码可读性高
  val ReducePEInputAllReady = Matrix.map(_.map(_.ReduceA.ready).reduce(_&&_)).reduce(_&&_) &&
    Matrix.map(_.map(_.ReduceB.ready).reduce(_&&_)).reduce(_&&_) &&
    Matrix.map(_.map(_.AddC.ready).reduce(_&&_)).reduce(_&&_)
  io.VectorA.ready := ReducePEInputAllReady
  io.VectorB.ready := ReducePEInputAllReady
  io.MatirxC.ready := ReducePEInputAllReady

  //全体PE对DC进行的完全同步的控制信号，保证每个PE的迭代器同步更新，保证每个Internal Reduce Dim的第一个次计算将C的数据送入PE，最后一次计算将PE的数据送回D
  io.ComputeGo := ReducePEInputAllReady

  val ReducePEConfigAllReady = Matrix.map(_.map(_.ConfigInfo.ready).reduce(_&&_)).reduce(_&&_)
  io.ConfigInfo.ready := ReducePEConfigAllReady

  //越浅的fifo，越少的能量消耗～
  //直接送到CscratchPad的数据，是最香的
  //只要MatrixD的是ready的，就可以送数据
  for (i <- 0 until Matrix_M){
    for (j <- 0 until Matrix_N){
      Matrix(i)(j).ResultD.ready := io.MatrixD.ready
    }
  }

  //ReducePE和MatrixTE需要一个对于externalReduce的处理，以提高热效率，提供主频，减少对CScratchPad的访问
  //ExternalReduce是指，我们的Scarchpad内的Tensor的K维大于1时，可以减少从CScratchPad的访问数据，让ReducePE使用自己暂存的累加结果后，再存至CScratchPad
  //Trick：再来，这里的K越大，我们的CSratchPad的平均访问次数就越少，就可以使用更慢更大的SRAM
  //256/8 = 32，在K大小为256时，我们的ExternalK可以是256/32 = 8,如果K=128，那么ExternalK=4。此时对CSratchPad进行一次读写的时间就只有4个周期，虽然好像也没有问题。
  //对于一个读+一个写端口的SRAM，我们确实可以在同一个周期内完成一次读一次写。但需要doublebuffer来阻挡从Memory到SRAM的数据。也就是K=32的情况，K=32的情况，用向量做Reduce就行了？

  //核心问题就是向量和矩阵算力的配比～～
  //向量算力和矩阵算力的配比，就是在roofline图上，找的那个说得通的点，证明我们的设计是合理的～

  //算的时候，reducePE可以对256的数据进行n种不同的计算，256/8 = 32。
  //这个(32,32)元素，送到PE里，可以得到(1,1)，(2,2)，(4,4)等的结果，影响ResultWidth的宽度，这会导致CScratchPad的访存带宽相应的提升
  //ResultWidth比较宽，导致CScratchpad宽度过宽，比如(4,4)的就需要(4*4)(4*4)*32bit,4096bit的读写，但是C有个好处就是不需要每周期都读写4096，可以花4个周期读，再画4个周期写。这样就是要求ExternalK必须大于4+4(单读写口的话)，也就是对于张量来说最小是(4+4)(8)。张量是16*16*64的张量。



}

