
package xiangshan.backend.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import boom.exu.ygjk._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile._
import boom.common.BoomBundle
// import freechips.rocketchip.regmapper.RRTest0Map

case class MMAccConfig()
case object MMAccKey extends Field[Option[MMAccConfig]](None)
case object BuildDMAygjk extends Field[Seq[Parameters => LazyRoCC]](Nil)
// class Withacc_MMacc extends Config((site,here,up) => {  
//     case BuildYGAC =>
//         (p:Parameters) => {          
//             val myAccel = Module(new MMacc)
//             myAccel
//         }
//     case MMAccKey => true
//     case BuildDMAygjk => true
//     }
// )

class CUTECrossingParams(
                          override val MemDirectMaster: TilePortParamsLike = TileMasterPortParams(where = MBUS)
                        ) extends RocketCrossingParams

class DebugInfoIO(implicit p: Parameters) extends BoomBundle with HWParameters{
  val DebugTimeStampe = UInt(64.W)
}

trait HWParameters{

  val YJPDebugEnable      = true
  val YJPADCDebugEnable   = true
  val YJPBDCDebugEnable   = true
  val YJPCDCDebugEnable   = true
  val YJPAMLDebugEnable   = true
  val YJPBMLDebugEnable   = true
  val YJPCMLDebugEnable   = true

  val YJPTASKDebugEnable  = true
  val YJPVECDebugEnable   = true
  val YJPMACDebugEnable   = true
  val YJPPEDebugEnable    = true
  val YJPAfterOpsDebugEnable    = true

  val ConvolutionApplicationConfigDataWidth = 32 //卷积相关的配置信息的宽度
  val ConvolutionDIM_Max = 65535 //卷积相关的配置信息的宽度
  val KernelSizeMax = 16 //卷积核的最大尺寸
  val StrideSizeMax = 4  //步长的最大尺寸
  //LLC的数据线宽度
  val LLCDataWidth = 256      //TODO:这个值需要从chipyard的config中来
  val LLCDataWidthByte = LLCDataWidth / 8
  //Memory的数据线宽度
  val MemoryDataWidth = 64    //TODO:这个值需要从chipyard的config中来
  //ReduceWidthByte 代表ReducePE进行内积时的数据宽度，单位是字节
  val ReduceWidthByte = LLCDataWidth / 8
  val ReduceWidth = ReduceWidthByte * 8
  //ResultWidthByte 代表ReducePE的结果宽度，单位是字节
  val ResultWidthByte = 4
  val ResultWidth = ResultWidthByte * 8

  val VectorWidth = 256   //向量流水线的宽度

  //最大可处理的程序的张量形状，
  val ApplicationMaxTensorSize = 16384
  val ApplicationMaxTensorSizeBitSize = log2Ceil(ApplicationMaxTensorSize) + 1
  //MMU的地址宽度
  val MMUAddrWidth = 64
  //MMU的数据线宽度
  val MMUDataWidth = ReduceWidth //TODO:ReduceWidth等于LLCDataWidth，以后得改
  //MMU的数据线有效数据位数
  val MMUDataWidthBitSize = log2Ceil(MMUDataWidth) + 1

  //LLC总线上的source最大数量 --> 这个参数和LLC的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必须大于LLC的访存延迟
  val LLCSourceMaxNum = 64
  val LLCSourceMaxNumBitSize = log2Ceil(LLCSourceMaxNum) + 1
  //Memory总线上的source最大数量 --> 这个参数和Memory的访存延迟强相关，若要满流水，这个sourceMAXnum的数量必顶大于Memory的访存延迟
  val MemorysourceMaxNum = 64
  val MemorysourceMaxNumBitSize = log2Ceil(MemorysourceMaxNum) + 1

  val SoureceMaxNum = math.max(LLCSourceMaxNum, MemorysourceMaxNum)
  val SoureceMaxNumBitSize = log2Ceil(SoureceMaxNum) + 1


  //Scaratchpad中保存的张量形状
  val Tensor_M = 64   //这里指要存的张量的M的大小
  val Tensor_N = 64   //这里指要存的张量的N的大小
  val Tensor_K = 2    //这里指要存的张量的K的ReduceVector的数量！不是张量的K的大小
  val Tensor_K_Element_Length = Tensor_K * ReduceWidthByte
  val ScaratchpadMaxTensorDim = Math.max(Tensor_M, Math.max(Tensor_N, Tensor_K))
  val ScaratchpadMaxTensorDimBitSize = log2Ceil(ScaratchpadMaxTensorDim) + 1
  //AScaratchpad中保存的张量形状为M*K
  //AScaratchpad的大小为Tenser_M * Tensor_K * ReduceWidthByte
  //128*(4*256/8)，单次读的张量为128*128的张量
  //单次计算需要的时间为(128/4)*(128/4)*4 = 4096拍，单次读需要128×4=512拍。
  //需要考虑Scaratchpad的顺序读，需要考虑为Scaratchpad分bank
  val AScratchpadSize = Tensor_M * Tensor_K * ReduceWidthByte //reduce
  val BScratchpadSize = Tensor_N * Tensor_K * ReduceWidthByte //reduce
  val CScratchpadSize = Tensor_M * Tensor_N * ResultWidthByte //result
  //Matrix_M，代表TE执行的矩阵乘法的M的大小
  val Matrix_M = 4
  //Matrix_N，代表TE执行的矩阵乘法的N的大小
  val Matrix_N = 4

  //目前的Scratchpad设计，分Tensor_T个bank，每次取Tensor_T个数据，根据取数逻辑，在不同的bank里取不同的数据，然后拼接

  val AScratchpadEntryByteSize = ReduceWidthByte //适合向TE供数的带宽
  val BScratchpadEntryByteSize = ReduceWidthByte
  val CScratchpadEntryByteSize = LLCDataWidthByte //这个比较合适的是LLC的带宽

  val AScratchpadNBanks = Matrix_M //注意这里与Matrix_M有强相关性，一般是Matrix_M的整数倍
  val BScratchpadNBanks = Matrix_N //这里与Matrix_N强相关
  val CScratchpadNBanks = Matrix_M*Matrix_N*ResultWidthByte/CScratchpadEntryByteSize //4*4*4/32 = 2//这里与Tensor_K强相关，同时读写任务～以及SRAM的各种参数

  val AScratchpad_Total_Bandwidth = AScratchpadNBanks * AScratchpadEntryByteSize  //ACSP的总带宽
  val BScratchpad_Total_Bandwidth = BScratchpadNBanks * BScratchpadEntryByteSize  //BCSP的总带宽
  val CScratchpad_Total_Bandwidth = CScratchpadNBanks * CScratchpadEntryByteSize  //CCSP的总带宽

  val AScratchpadBankSize = AScratchpadSize / AScratchpadNBanks
  val BScratchpadBankSize = BScratchpadSize / BScratchpadNBanks
  val CScratchpadBankSize = CScratchpadSize / CScratchpadNBanks
  val AScratchpadBankNEntrys = AScratchpadBankSize / AScratchpadEntryByteSize
  val BScratchpadBankNEntrys = BScratchpadBankSize / BScratchpadEntryByteSize
  val CScratchpadBankNEntrys = CScratchpadBankSize / CScratchpadEntryByteSize


  //MACLatency 用于ReducePE内的乘累加树的延迟描述
  val MAC32TreeLevel = log2Ceil(ReduceWidthByte * 8 / 32)
  val MAC32Latency = 3 //这是一个经验值，依据时序结果，填写的需要切分的流水段数量
  val MAC16TreeLevel = log2Ceil(ReduceWidthByte * 8 / 16)
  val MAC16Latency = 4
  val MAC8TreeLevel = log2Ceil(ReduceWidthByte * 8 / 8)
  val MAC8Latency = 5
  //乘累加FIFO的深度
  val ResultFIFODepth = 8
  val InputFIFODepth = 8

  val CMemoryLoaderReadFromScratchpadFIFODepth = 16 //这个fifo不够深的话，会导致读请求排不满，必须和LLC的回数延迟一致
  val CMemoryLoaderReadFromMemoryFIFODepth = 16 //这个fifo不够深的话，会导致读请求排不满，必须和LLC的回数延迟一致

  val VecTaskInstBufferDepth = 32 //VecTask的指令缓冲深度
  val VecTaskInstBufferSize = 8 //VecTask的指令缓冲的数量
  val VecTaskDataBufferDepth = 4 //VecTask的指令缓冲深度掩盖从VecInterface到VPU的数据传输延迟即可
}

//需要配置的信息：oc -- 控制器发来的oc编号, 
//                ic, oh, ow, kh, kw, ohb -- 外层循环次数,
//                icb -- 矩阵乘计算中的中间长度
//                paddingH, paddingW, strideH, strideW -- 卷积层属性

class TaskCtrlInfo(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ADC = (new Bundle {
    // val TaskWorking = Valid(Bool())
    val TaskEnd = DecoupledIO(Bool())
    val ComputeEnd = Flipped(DecoupledIO(Bool()))
  })
  val BDC = (new Bundle {
    // val TaskWorking = Valid(Bool())
    val TaskEnd = DecoupledIO(Bool())
    val ComputeEnd = Flipped(DecoupledIO(Bool()))
  })
  val CDC = (new Bundle {
    // val TaskWorking = Valid(Bool())
    val TaskEnd = DecoupledIO(Bool())
    val ComputeEnd = Flipped(DecoupledIO(Bool()))
  })

  val AML = (new Bundle {
    // val TaskWorking = Valid(Bool())
    val TaskEnd = DecoupledIO(Bool())
    val LoadEnd = Flipped(DecoupledIO(Bool()))
  })

  val BML = (new Bundle {
    // val TaskWorking = Valid(Bool())
    val TaskEnd = DecoupledIO(Bool())
    val LoadEnd = Flipped(DecoupledIO(Bool()))
  })

  val CML = (new Bundle {
    // val TaskWorking = Valid(Bool())
    val TaskEnd = DecoupledIO(Bool())
    val LoadEnd = Flipped(DecoupledIO(Bool()))
  })

  val ScaratchpadChosen = (new Bundle {
    val ADataControllerChosenIndex = UInt(1.W)
    val BDataControllerChosenIndex = UInt(1.W)
    val CDataControllerChosenIndex = UInt(1.W)

    val AMemoryLoaderChosenIndex = UInt(1.W)
    val BMemoryLoaderChosenIndex = UInt(1.W)
    val CMemoryLoaderChosenIndex = UInt(1.W)
  })
}

class AfterOpsInterface(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{

  //每拍可接受一个来自CDC的与SCP和TE等宽的数据，并在自己模块内完成数据的拆分、重排、缩放、转置以及其他复杂向量任务
  val CDCDataToInterface     = DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W))
  val InterfaceToCDCData     = Flipped(DecoupledIO(UInt((ResultWidth*Matrix_M*Matrix_N).W)))
  val CDCStoreAddr                        = Output(UInt(log2Ceil(CScratchpadBankNEntrys).W))

  val VecInstQueueID = UInt(1.W)
}

class VPUInterface_Input(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val inst_uop = Output(UInt(32.W))
  val inst_src0 = Output(UInt(VectorWidth.W))
  val inst_src1 = Output(UInt(VectorWidth.W))
  val inst_src0_type = Output(UInt(2.W))//从寄存器还是来自输入
  val inst_src1_type = Output(UInt(2.W))//从寄存器还是来自输入
  val inst_dest_type = Output(UInt(2.W))//写回寄存器还是写回输出
  val stream_id = Output(UInt(log2Ceil(Matrix_M*Matrix_N+10).W))//stream data的id
}

class VPUInterface_Output(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val stream_id = Output(UInt(log2Ceil(Matrix_M*Matrix_N+10).W))//stream data的id
  val stream_data = Output(UInt(VectorWidth.W))//stream data还能存一些额外的信息，这些信息也会返回，后续可以用于配置VPU的部分隐式寄存器，或者留存在VPU的的隐式寄存器中，这些寄存器是uop可见的，如下一次的scale，下一次的bias等。
}

class VPUInterfaceIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val VPU_Input = (DecoupledIO(new VPUInterface_Input))
  val VPU_Output = Flipped(DecoupledIO(new VPUInterface_Output))
}


class VectorInterfaceIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{

  //每拍可接受一个来自AfterOpsInterface的与VectorWidth等宽的数据
  val VecTask = DecoupledIO(UInt(log2Ceil(VecTaskInstBufferSize).W))
  // val VecInterfaceReady = Output(Bool())
  val VectorDataIn     = DecoupledIO(UInt((VectorWidth).W))
  val VectorTOVPU     = (new VPUInterfaceIO)
  val VectorDataOut     = Flipped(DecoupledIO(UInt((VectorWidth).W)))
}


case object StreamStateType extends Field[UInt]{
  val StreamStateTypeBitWidth = 4

  val NoReorder = 0.U(StreamStateTypeBitWidth.W)
  val Reorder_DIM_N_First = 1.U(StreamStateTypeBitWidth.W)
  val Reorder_DIM_M_First = 2.U(StreamStateTypeBitWidth.W)
}

//描述计算时，数据流的访问顺序，transpose的时候就是N_M，不transpose的时候就是M_N
case object CaculateStreamStateType extends Field[UInt]{
  val CaculateStreamStateTypeBitWidth = 4

  val M_N = 0.U(CaculateStreamStateTypeBitWidth.W)
  val N_M = 1.U(CaculateStreamStateTypeBitWidth.W)
}

class CUTE_uop(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val Stream_state = UInt((StreamStateType.StreamStateTypeBitWidth).W)
  val Stream_uop = UInt(32.W)
  val Element_uop = UInt(32.W)
  val Vector_uop = UInt(32.W)


}

class AfterOpsMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ApplicationTensor_C = (new Bundle{
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ApplicationTensor_D = (new Bundle{
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  //接受后操作的任务，有可能是重排序，有可能是缩放，有可能是转置，有可能是其他复杂后操作任务
  val Is_Transpose                        = (Bool())      //是否需要转置
  val Is_Reorder_Only_Ops                 = (Bool())      //是否只是重排，不需要计算
  val Is_EasyScale_Only_Ops               = (Bool())      //是否只是简单的缩放，不需要额外的后操作计算
  val Is_VecFIFO_Ops                      = (Bool())      //是否真的需要通用VecFIFO的参与

  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成

  val CUTEuop                         = new CUTE_uop
}

class ADCMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ApplicationTensor_A = (new Bundle{
    // val ApplicationTensor_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  val Is_Transpose                        = (Bool())      //是否需要转置

  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class BDCMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ApplicationTensor_B = (new Bundle{
    // val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  val Is_Transpose                        = (Bool())      //是否需要转置

  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                        = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class CDCMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ApplicationTensor_C = (new Bundle{
    // val ApplicationTensor_C_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_C_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ApplicationTensor_D = (new Bundle{
    // val ApplicationTensor_D_BaseVaddr   = (UInt(MMUAddrWidth.W))
    // val BlockTensor_D_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  val Is_Transpose                        = (Bool())      //是否需要转置
  val Is_AfterOps_Tile                    = (Bool())      //是否是需要执行后操作的Tile，包括转置等

  val Is_Reorder_Only_Ops                 = (Bool())      //是否只是重排，不需要计算
  val Is_EasyScale_Only_Ops               = (Bool())      //是否只是简单的缩放，不需要额外的后操作计算
  val Is_VecFIFO_Ops                      = (Bool())      //是否真的需要通用VecFIFO的参与



  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
  val MicroTask_TEComputeEndValid         = Flipped(Bool())//已完成当前的TE的计算任务(但是还没有完成后操作)，但是可以提前释放TE的占用
  val MicroTask_TEComputeEndReady         = (Bool())       //已知晓当前的TE的计算任务完成
}

class AMLMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ApplicationTensor_A = (new Bundle{
    val ApplicationTensor_A_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_A_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_A_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val Convolution_OH_DIM_Length       = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_OW_DIM_Length       = (UInt(log2Ceil(ConvolutionDIM_Max).W))
    val Convolution_Stride_H            = (UInt(log2Ceil(StrideSizeMax).W))
    val Convolution_Stride_W            = (UInt(log2Ceil(StrideSizeMax).W))
    val Convolution_KH_DIM_Length       = (UInt(log2Ceil(KernelSizeMax).W))
    val Convolution_KW_DIM_Length       = (UInt(log2Ceil(KernelSizeMax).W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  //知道卷积核的位置和当前的OHOW，确认是否需要padding进行0填充
  val Convolution_Current_OH_Index        = (UInt(log2Ceil(ConvolutionDIM_Max).W))
  val Convolution_Current_OW_Index        = (UInt(log2Ceil(ConvolutionDIM_Max).W))
  val Convolution_Current_KH_Index        = (UInt(log2Ceil(KernelSizeMax).W))
  val Convolution_Current_KW_Index        = (UInt(log2Ceil(KernelSizeMax).W))

  val Conherent                           = (Bool())      //是否需要coherent

  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                        = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class BMLMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  val ApplicationTensor_B = (new Bundle{
    val ApplicationTensor_B_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_B_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_B_Stride_N    = (UInt(MMUAddrWidth.W))//下一个N需要增加多少的地址偏移量
    val Convolution_OC_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
    val Convolution_KH_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
    val Convolution_KW_DIM_Length       = (UInt(ConvolutionApplicationConfigDataWidth.W))
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  //知道卷积核的位置，确认kernel的具体BlockTensor_B_BaseVaddr，那这个不需要传进来，TaskCtrl算完送进来就行了。
  val Convolution_Current_KH_Index        = (UInt(ConvolutionApplicationConfigDataWidth.W))
  val Convolution_Current_KW_Index        = (UInt(ConvolutionApplicationConfigDataWidth.W))

  val Conherent                           = (Bool())      //是否需要coherent

  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                   = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}


class CMLMicroTaskConfigIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{
  //就是一个TensorC，是累加寄存器视角的不动的部分
  val ApplicationTensor_C = (new Bundle{
    val ApplicationTensor_C_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_C_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_C_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val ApplicationTensor_D = (new Bundle{
    val ApplicationTensor_D_BaseVaddr   = (UInt(MMUAddrWidth.W))
    val BlockTensor_D_BaseVaddr         = (UInt(MMUAddrWidth.W))
    val ApplicationTensor_D_Stride_M    = (UInt(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
    val dataType                        = (UInt(ElementDataType.DataTypeBitWidth.W))
  })

  val LoadTaskInfo = (new Bundle{
    val Is_ZeroLoad = (Bool())
    val Is_RepeatRowLoad = (Bool())
    val Is_FullLoad = (Bool())
  })

  val StoreTaskInfo = (new Bundle{
    val Is_ZeroStore = (Bool())//暂时没有传递的参数
  })

  val Is_Transpose                        = (Bool())      //是否需要转置
  val ScaratchpadTensor_M                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_K                 = (UInt(ScaratchpadMaxTensorDimBitSize.W))

  val IsLoadMicroTask                     = (Bool())      //是否是Load任务
  val IsStoreMicroTask                    = (Bool())      //是否是Store任务

  val MicroTaskReady                      = Flipped(Bool())//可配置下一个任务
  val MicroTaskValid                      = (Bool())       //当前任务的配置信息有效
  val MicroTaskEndValid                        = Flipped(Bool())//已完成当前任务
  val MicroTaskEndReady                   = (Bool())       //已知晓当前任务完成
}

class ConfigInfoIO(implicit p: Parameters) extends BoomBundle with HWParameters with YGJKParameters{

  val MMUConfig = Flipped(new MMUConfigIO)
  val ApplicationTensor_A = (new Bundle{
    val ApplicationTensor_A_BaseVaddr = (UInt(MMUAddrWidth.W))
    val BlockTensor_A_BaseVaddr       = (UInt(MMUAddrWidth.W))
    val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
    val Conherent                     = (Bool())
  })

  val ApplicationTensor_B = (new Bundle{
    val ApplicationTensor_B_BaseVaddr = (UInt(MMUAddrWidth.W))
    val BlockTensor_B_BaseVaddr       = (UInt(MMUAddrWidth.W))
    val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
    val Conherent                     = (Bool())
  })

  val ApplicationTensor_C = (new Bundle{
    val ApplicationTensor_C_BaseVaddr = (UInt(MMUAddrWidth.W))
    val BlockTensor_C_BaseVaddr       = (UInt(MMUAddrWidth.W))
    val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
    val Conherent                     = (Bool())
  })
  val ApplicationTensor_D = (new Bundle{
    val ApplicationTensor_D_BaseVaddr = (UInt(MMUAddrWidth.W))
    val BlockTensor_D_BaseVaddr       = (UInt(MMUAddrWidth.W))
    val MemoryOrder                   = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
    val Conherent                     = (Bool())
  })
  val ApplicationTensor_M = (UInt(ApplicationMaxTensorSizeBitSize.W))
  val ApplicationTensor_N = (UInt(ApplicationMaxTensorSizeBitSize.W))
  val ApplicationTensor_K = (UInt(ApplicationMaxTensorSizeBitSize.W))

  val ScaratchpadTensor_M = (UInt(ScaratchpadMaxTensorDimBitSize.W)) //Scaratchpad当前处理的矩阵乘的M
  val ScaratchpadTensor_N = (UInt(ScaratchpadMaxTensorDimBitSize.W)) //Scaratchpad当前处理的矩阵乘的N
  val ScaratchpadTensor_K = (UInt(ScaratchpadMaxTensorDimBitSize.W)) //Scaratchpad当前处理的矩阵乘的K

  val ComputeGo = (Bool())


  val dataType = (UInt(ElementDataType.DataTypeBitWidth.W)) //0-矩阵乘，1-卷积
  val taskType = (UInt(CUTETaskType.CUTETaskBitWidth.W)) //1-32位，2-16位， 4-32位
  // val ExternalReduceSize = (UInt(ScaratchpadMaxTensorDimBitSize.W))
  val CMemoryLoaderConfig = (new Bundle{
    val MemoryOrder = (UInt(MemoryOrderType.MemoryOrderTypeBitWidth.W))
    val TaskType = (UInt(CMemoryLoaderTaskType.TypeBitWidth.W))
  })

}

//从Scaratchpad中取数，要明确是从哪个bank里，取第几行的数据，然后完成数据拼接返回
//从哪个bank里取数据，取第几行的数据，是由datacontrol模块算出来的
//怎么在bank里编排数据，是由MemoryLoader模块填进去的
//MemoryLoader模块和datacontrol模块都有窗口期，可以完成数据额外的一些编排如量化、反稀疏、反量化、量化重排等等
//将MemoryLoader模块和datacontrol模块分开，是为了使用窗口期，让单读写口的ScarchPad可以独立运行
//有没有能同时读写的SRAM啊？我能保证不写同一块数据,还是先doublebuffer吧....
//我们考虑到回数的延迟，所以DataControl与Scarachpad之间也是有fifo的。考虑到后续的SRAM是一个简单模块，fifo要加在DataControl里，让Scarachpad尽可能简单。
class ADataControlScaratchpadIO extends Bundle with HWParameters{
  //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
  val BankAddr = Flipped(DecoupledIO(Vec(AScratchpadNBanks, (UInt(log2Ceil(AScratchpadBankNEntrys).W)))))
  //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
  val Data = Valid(Vec(AScratchpadNBanks, UInt(ReduceWidth.W)))
  //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
  // val Chosen = Input(Bool())
}

class AMemoryLoaderScaratchpadIO extends Bundle with HWParameters{
  //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
  val BankId = Flipped(Valid(UInt(log2Ceil(AScratchpadNBanks).W)))
  val BankAddr = Flipped(Valid(UInt(log2Ceil(AScratchpadBankNEntrys).W)))
  //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
  val Data = Flipped(Valid(UInt(ReduceWidth.W)))
  //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
  // val Chosen = Input(Bool())
}

class BDataControlScaratchpadIO extends Bundle with HWParameters{
  //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
  val BankAddr = Flipped(DecoupledIO(Vec(BScratchpadNBanks, (UInt(log2Ceil(BScratchpadBankNEntrys).W)))))
  //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
  val Data = Valid(Vec(BScratchpadNBanks, UInt(ReduceWidth.W)))
  //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
  // val Chosen = Input(Bool())
}

class BMemoryLoaderScaratchpadIO extends Bundle with HWParameters{
  //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
  val BankId = Flipped(Valid(UInt(log2Ceil(BScratchpadNBanks).W)))
  val BankAddr = Flipped(Valid(UInt(log2Ceil(BScratchpadBankNEntrys).W)))
  //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是ReduceWidthByte*8
  val Data = Flipped(Valid(UInt(ReduceWidth.W)))
  //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
  // val Chosen = Input(Bool())
}


class CDataControlScaratchpadIO extends Bundle with HWParameters{
  //bankaddr是对nbanks个bank，各自bank的行选信号,是一个vec，有nbanks个元素，每个元素是一个UInt，UInt的宽度是log2Ceil(AScratchpadBankNLines)，是输入的需要握手的数据
  val ReadBankAddr = Flipped(Valid(Vec(CScratchpadNBanks, (UInt(log2Ceil(CScratchpadBankNEntrys).W)))))
  val WriteBankAddr = Flipped(Valid(Vec(CScratchpadNBanks, (UInt(log2Ceil(CScratchpadBankNEntrys).W)))))
  //bankdata是对nbanks个bank，各自bank的行数据，是一个vec，有nbanks个元素，每个元素是一个UInt
  val ReadResponseData = Valid(Vec(CScratchpadNBanks, UInt(LLCDataWidth.W)))
  val WriteRequestData = Flipped(Valid(Vec(CScratchpadNBanks, UInt(LLCDataWidth.W))))
  //chosen是选择该ScarchPad的信号，是一个bool，我们做doublebuffer，选择其一供数，选择其一加载数据
  val ReadWriteRequest = Input(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
  val ReadWriteResponse = Output(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
  // val Chosen = Input(Bool())
}

class CMemoryLoaderScaratchpadIO extends Bundle with HWParameters{
  val ReadRequestToScarchPad = (new Bundle{
    val FullBankLoad = Input(Bool())
    val BankId = Flipped(Valid(UInt(log2Ceil(CScratchpadNBanks).W)))
    val BankAddr = Flipped(Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W)))
    val ReadResponseData = (Valid(Vec(CScratchpadNBanks, UInt(LLCDataWidth.W))))
  })
  val WriteRequestToScarchPad = (new Bundle{
    // val BankId = Flipped(Valid(UInt(log2Ceil(CScratchpadNBanks).W)))
    val BankAddr = Flipped(Valid(UInt(log2Ceil(CScratchpadBankNEntrys).W)))
    val Data = Flipped(Valid(Vec(CScratchpadNBanks, UInt(LLCDataWidth.W))))
  })

  val ReadWriteRequest = Input(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
  val ReadWriteResponse = Output(UInt((ScaratchpadTaskType.TaskTypeBitWidth).W))
  // val Chosen = Input(Bool())
}

//LocalMMU的接口
class LocalMMUIO extends Bundle with HWParameters{

  //发出的访存请求
  val Request = Flipped(DecoupledIO(new Bundle{
    val RequestVirtualAddr = UInt(MMUAddrWidth.W)
    val RequestConherent = Bool()
    val RequestData = UInt(MMUDataWidth.W)
    val RequestSourceID = UInt(SoureceMaxNumBitSize.W)
    val RequestType_isWrite = Bool()
  }))
  //读请求分发到的TL Link的事务编号
  val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
  val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

  //Memoryloader一定能保证收回！
  val Response = Valid(new Bundle{
    val ReseponseData = UInt(MMUDataWidth.W)
    val ReseponseConherent = Bool()
    val ReseponseSourceID = UInt(SoureceMaxNumBitSize.W)
  })
}

class MMU2TLIO extends Bundle with HWParameters{

  //发出的访存请求
  val Request = Flipped(DecoupledIO(new Bundle{
    val RequestPhysicalAddr = UInt(MMUAddrWidth.W)
    val RequestConherent = Bool()
    val RequestData = UInt(MMUDataWidth.W)
    val RequestSourceID = UInt(SoureceMaxNumBitSize.W)
    val RequestType_isWrite = Bool()
  }))
  //读请求分发到的TL Link的事务编号
  val ConherentRequsetSourceID = Valid(UInt(LLCSourceMaxNumBitSize.W))
  val nonConherentRequsetSourceID = Valid(UInt(MemorysourceMaxNumBitSize.W))

  //Memoryloader一定能保证收回！
  val Response = Valid(new Bundle{
    val ReseponseData = UInt(MMUDataWidth.W)
    val ReseponseConherent = Bool()
    val ReseponseSourceID = UInt(SoureceMaxNumBitSize.W)
  })
}


//数据类型的样板类
case object  ElementDataType extends Field[UInt]{
  val DataTypeBitWidth = 3
  val DataTypeUndef  = 0.U(DataTypeBitWidth.W)
  val DataTypeWidth32 = 4.U(DataTypeBitWidth.W)
  val DataTypeWidth16 = 2.U(DataTypeBitWidth.W)
  val DataTypeWidth8  = 1.U(DataTypeBitWidth.W)
}

//工作任务的样板类
case object  CUTETaskType extends Field[UInt]{
  val CUTETaskBitWidth = 8
  val TaskTypeUndef = 0.U(CUTETaskBitWidth.W)
  val TaskTypeMatrixMul = 1.U(CUTETaskBitWidth.W)
  val TaskTypeConv = 2.U(CUTETaskBitWidth.W)
}

case object  CMemoryLoaderTaskType extends Field[UInt]{
  val TypeBitWidth = 8
  val TaskTypeUndef = 0.U(TypeBitWidth.W)
  val TaskTypeTensorStore = 1.U(TypeBitWidth.W)
  val TaskTypeTensorLoad = 2.U(TypeBitWidth.W)
  val TaskTypeTensorZeroLoad = 3.U(TypeBitWidth.W) //直接将数据填充为0，实际上是什么也没做，默认可以写入SRAM，无视以前SRAM里面的数据即可
}
case object  MemoryOrderType extends Field[UInt]{
  val MemoryOrderTypeBitWidth = 8
  val OrderTypeUndef      = 0.U(MemoryOrderTypeBitWidth.W)
  val OrderType_Mb_Kb     = 1.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Mb在前，Kb在后
  val OrderType_Mb_Nb     = 1.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Mb在前，Nb在后
  val OrderType_Nb_Kb     = 1.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Nb在前，Kb在后
  val OrderType_Nb_Mb     = 2.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Nb在前，Mb在后
  val OrderType_Kb_Mb     = 2.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Kb在前，Mb在后
  val OrderType_Kb_Nb     = 2.U(MemoryOrderTypeBitWidth.W) //在地址空间中顺序摆放的顺序, Kb在前，Nb在后

}


case object ScaratchpadTaskType extends Field[UInt]{
  val TaskTypeBitWidth = 4    //对于单个Scaratchpad，其并发的数据来源一共用3个，所以用3bit来表示。1.DataController对PE的输入数据的对ScarchPad读请求 2.DataController将PE的输出结果送入ScaratchPad写请求 3。MemoryLoader对ScarchPad的写请求
  //我们不知道Scaratchpad的读写端口数量，所以用使能信号表示接受的数据来源
  val EnableReadFromDataController = 1.U(TaskTypeBitWidth.W)
  val EnableWriteFromDataController = 2.U(TaskTypeBitWidth.W)
  val EnableWriteFromMemoryLoader = 4.U(TaskTypeBitWidth.W)
  val EnableReadFromMemoryLoader = 8.U(TaskTypeBitWidth.W)
  val ReadFromDataControllerIndex = 0
  val WriteFromDataControllerIndex = 1
  val WriteFromMemoryLoaderIndex = 2
  val ReadFromMemoryLoaderIndex = 3
}

class ScaratchpadTask extends Bundle with HWParameters{
  // * Elements defined earlier in the Bundle are higher order upon
  // * serialization. For example:
  // *   val bundle = Wire(new MyBundle)
  // *   bundle.foo := 0x1234.U
  // *   bundle.bar := 0x5678.U
  // *   val uint = bundle.asUInt
  // *   assert(uint === "h12345678".U) // This will pass
  val ReadFromMemoryLoader = Bool()
  val WriteFromMemoryLoader = Bool()
  val WriteFromDataController = Bool()
  val ReadFromDataController = Bool()
}

case object LocalMMUTaskType extends Field[UInt]{
  val TaskTypeBitWidth = 2
  val TaskTypeMax = 3
  val AFirst = 0.U(TaskTypeBitWidth.W)
  val BFirst = 1.U(TaskTypeBitWidth.W)
  val CFirst = 2.U(TaskTypeBitWidth.W)
  // val DFirst = 3.U(TaskTypeBitWidth.W)
}