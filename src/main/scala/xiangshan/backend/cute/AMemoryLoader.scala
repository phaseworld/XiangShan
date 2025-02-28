
package xiangshan.backend.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._

//AMemoryLoader，用于加载A矩阵的数据，供给Scratchpad使用
//从不同的存储介质中加载数据，供给Scratchpad使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据Scarchpad的设计，完成Tensor的切分以及将数据的填入Scaratchpad

//注意，数据的reorder是可以离线完成的！这也属于编译器的一环。

class ASourceIdSearch extends Bundle with HWParameters{
  val ScratchpadBankId = UInt(log2Ceil(AScratchpadNBanks).W)
  val ScratchpadAddr = UInt(log2Ceil(AScratchpadBankNEntrys).W)
}

//本模块的核心任务是从外部接口加载数据到Scarchpad中
//所有的访存任务会通过TL-Link接口发出，每次的读请求的宽度是ReduceWidthByte

//总写回SCP的次数 = Tensor_M * Tensor_K
//需要解决的核心问题为，计算出每次访存的地址，然后发出访存请求，然后接受访存的返回值，然后将返回值写入到Scarchpad中
//1.计算访存地址，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.每次Load的微任务，KH_Index、KW_Index不变，根据OH、OW、stride_H、stride_W即可得到当前Load任务的起始地址。
////b.我们的卷积数据的Input是[NHW][C]排布的,所以ApplicationTensor_A_Stride_M就是下一个[NHW]的地址偏移量
////c.每次迭代下一个Load请求，IH_Index、IW_Index会变，根据IH_Index、IW_Index，即可得到当前的Load任务是否需要发生真实的Load请求，还是直接发生0填充
//2.发出访存请求，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.需要记录mmu发送的source_id，用于后续的数据写回，这里的source_id是唯一的，且会记录该source_id对应的Scarchpad的地址和bank号
////b.需要0填充的情况，需要单独处理，找机会和某一次写回合并，根据此刻某个bank是否被占用来偷时点(由于SCP的总读写带宽肯定比LLC的总带宽大，所以这里的偷时点是可行的)
//3.接受访存返回值，需要考虑当前的IH、IW、KH、KW、stride_H、stride_W，来计算出当前要load的地址
////a.需要根据source_id找到对应的Scarchpad的地址和bank号
////b.同时找机会和某一次写回合并！根据此刻某个bank是否被占用来偷时点写回SCP

//重要的几个逻辑部分
//1.计算IH、IW是否超界，如果超界，需要进行0填充，而不是发出访存请求(这里可能是时序不满足的点，如果不满足，可以提前就算好)
//2.0填充的逻辑，需要单独处理，需要找机会和某一次写回合并。故需要一个NACK寄存器来记录哪个bank此刻有ZeroFill任务，如果无法记录NACK任务，则停止发出访存请求，直到有空闲的bank

class AMemoryLoader(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new Bundle{
    //先整一个ScarchPad的接口的总体设计
    val ToScarchPadIO = Flipped(new AMemoryLoaderScaratchpadIO)
    val ConfigInfo = Flipped(new AMLMicroTaskConfigIO)
    val LocalMMUIO = Flipped(new LocalMMUIO)
    val DebugInfo = Input(new DebugInfoIO)
  })
  //TODO:init
  io.ToScarchPadIO.BankAddr.valid := false.B
  io.ToScarchPadIO.BankAddr.bits := 0.U
  io.ToScarchPadIO.BankId.valid := false.B
  io.ToScarchPadIO.BankId.bits := 0.U
  io.ToScarchPadIO.Data.valid := false.B
  io.ToScarchPadIO.Data.bits := 0.U
  io.LocalMMUIO.Request.valid := false.B
  io.LocalMMUIO.Request.bits := DontCare

  io.ConfigInfo.MicroTaskEndValid := false.B
  io.ConfigInfo.MicroTaskReady := false.B


  val ScaratchpadBankAddr = io.ToScarchPadIO.BankAddr
  val ScaratchpadData = io.ToScarchPadIO.Data

  val ConfigInfo = io.ConfigInfo

  val Tensor_A_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))
  val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

  val ApplicationTensor_A_Stride_M    = RegInit(0.U(MMUAddrWidth.W))//下一个M需要增加多少的地址偏移量
  val Convolution_OH_DIM_Length       = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W)) //对于卷积来说，OH的值是多少，用于我们进行地址的计算
  val Convolution_OW_DIM_Length       = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W)) //对于卷积来说，OW的值是多少，用于我们进行地址的计算
  val Convolution_Stride_H            = RegInit(0.U(log2Ceil(StrideSizeMax).W)) //对于卷积来说，stride是多少，用于我们进行地址的计算
  val Convolution_Stride_W            = RegInit(0.U(log2Ceil(StrideSizeMax).W)) //对于卷积来说，stride是多少，用于我们进行地址的计算
  val Convolution_KH_DIM_Length       = RegInit(0.U(log2Ceil(KernelSizeMax).W)) //对于卷积来说，KH的值是多少，用于我们进行地址的计算
  val Convolution_KW_DIM_Length       = RegInit(0.U(log2Ceil(KernelSizeMax).W)) //对于卷积来说，KW的值是多少，用于我们进行地址的计算
  val dataType                        = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))  //数据类型

  val ScaratchpadTensor_M                 = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))//需要加载到Scaratchpad的Tensor的M
  val ScaratchpadTensor_K                 = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))//需要加载到Scaratchpad的Tensor的K

  //知道卷积核的位置和当前的OHOW，确认是否需要padding进行0填充
  val Convolution_Current_OH_Index        = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
  val Convolution_Current_OW_Index        = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
  val Init_Convolution_Current_OH_Index   = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
  val Init_Convolution_Current_OW_Index   = RegInit(0.U(log2Ceil(ConvolutionDIM_Max).W))
  val Convolution_Current_KH_Index        = RegInit(0.U(log2Ceil(KernelSizeMax).W))
  val Convolution_Current_KW_Index        = RegInit(0.U(log2Ceil(KernelSizeMax).W))

  //任务状态机,顺序读取所有分块矩阵
  val s_idle :: s_mm_task :: s_end :: Nil = Enum(3)
  val state = RegInit(s_idle)

  //访存状态机，用来配合流水线刷新
  val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
  val memoryload_state = RegInit(s_load_idle)
  val MemoryOrder_LoadConfig = RegInit(MemoryOrderType.OrderType_Mb_Kb)

  val IH_Stride = RegInit(0.U(log2Ceil(MMUAddrWidth).W))
  val IW_Stride = RegInit(0.U(log2Ceil(MMUAddrWidth).W))

  val Conherent = RegInit(true.B) //是否一致性访存的标志位，由TaskController提供

  val Convolution_IW_DIM_Length = Convolution_OW_DIM_Length * Convolution_Stride_W//可以提前算，存成Reg
  val Convolution_IH_DIM_Length = Convolution_OH_DIM_Length * Convolution_Stride_H//可以提前算，存成Reg

  //允许每个bank最多1个nack，这样保证只要有bank空闲我们都能写入数据，同时保证了Load请求的译码不停顿。
  val NACK_ZeroFill_Hloding_Reg = RegInit((VecInit(Seq.fill(AScratchpadNBanks)(0.U((new ASourceIdSearch).getWidth.W)))))//每个bank的NACK值
  val NACK_ZeroFill_Hloding_Valid = RegInit(VecInit(Seq.fill(AScratchpadNBanks)(false.B)))//每个bank的NACK计数器是否有效
  val Zero_Fill_TableItem = Wire((VecInit(Seq.fill(AScratchpadNBanks)(0.U((new ASourceIdSearch).getWidth.W)))))
  val Zero_Fill_TableItem_Valid = Wire(VecInit(Seq.fill(AScratchpadNBanks)(false.B)))

  //如果configinfo有效
  //状态机
  when(state === s_idle){
    //idel状态才可以接受新的配置信息
    ConfigInfo.MicroTaskReady := true.B
    when(ConfigInfo.MicroTaskReady && ConfigInfo.MicroTaskValid){
      //当前配置的指令有效
      state := s_mm_task
      memoryload_state := s_load_init
      ScaratchpadTensor_M := ConfigInfo.ScaratchpadTensor_M
      ScaratchpadTensor_K := ConfigInfo.ScaratchpadTensor_K

      Tensor_A_BaseVaddr := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr
      Tensor_Block_BaseAddr := ConfigInfo.ApplicationTensor_A.BlockTensor_A_BaseVaddr
      // Conherent := io.ConfigInfo.bits.ApplicationTensor_A.Conherent

      ApplicationTensor_A_Stride_M := ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M
      Convolution_OH_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_OH_DIM_Length
      Convolution_OW_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_OW_DIM_Length
      Convolution_Stride_H := ConfigInfo.ApplicationTensor_A.Convolution_Stride_H
      Convolution_Stride_W := ConfigInfo.ApplicationTensor_A.Convolution_Stride_W
      Convolution_KH_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_KH_DIM_Length
      Convolution_KW_DIM_Length := ConfigInfo.ApplicationTensor_A.Convolution_KW_DIM_Length
      dataType := ConfigInfo.ApplicationTensor_A.dataType

      Init_Convolution_Current_OH_Index := ConfigInfo.Convolution_Current_OH_Index
      Init_Convolution_Current_OW_Index := ConfigInfo.Convolution_Current_OW_Index
      Convolution_Current_OH_Index := ConfigInfo.Convolution_Current_OH_Index
      Convolution_Current_OW_Index := ConfigInfo.Convolution_Current_OW_Index
      Convolution_Current_KH_Index := ConfigInfo.Convolution_Current_KH_Index
      Convolution_Current_KW_Index := ConfigInfo.Convolution_Current_KW_Index

      IH_Stride :=  Convolution_IW_DIM_Length * ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M //每移动一次IH，需要增加的地址偏移量
      IW_Stride :=  ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M //每移动一次IW，需要增加的地址偏移量
      assert(ConfigInfo.ScaratchpadTensor_K === Tensor_K.U)
      //
      if(YJPAMLDebugEnable)
      {
        printf("[AML<%d>]AMemoryLoader Task Start\n",io.DebugInfo.DebugTimeStampe)
        //输出所有配置项
        printf("[AML<%d>]ScaratchpadTensor_M:%d, ScaratchpadTensor_K:%d, Tensor_A_BaseVaddr:%x, Tensor_Block_BaseAddr:%x, ApplicationTensor_A_Stride_M:%x, Convolution_OH_DIM_Length:%d, Convolution_OW_DIM_Length:%d, Convolution_Stride_H:%d, Convolution_Stride_W:%d, Convolution_KH_DIM_Length:%d, Convolution_KW_DIM_Length:%d, dataType:%d, Convolution_Current_OH_Index:%d, Convolution_Current_OW_Index:%d, Convolution_Current_KH_Index:%d, Convolution_Current_KW_Index:%d\n",io.DebugInfo.DebugTimeStampe,ConfigInfo.ScaratchpadTensor_M,ConfigInfo.ScaratchpadTensor_K,ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr,ConfigInfo.ApplicationTensor_A.BlockTensor_A_BaseVaddr,ConfigInfo.ApplicationTensor_A.ApplicationTensor_A_Stride_M,ConfigInfo.ApplicationTensor_A.Convolution_OH_DIM_Length,ConfigInfo.ApplicationTensor_A.Convolution_OW_DIM_Length,ConfigInfo.ApplicationTensor_A.Convolution_Stride_H,ConfigInfo.ApplicationTensor_A.Convolution_Stride_W,ConfigInfo.ApplicationTensor_A.Convolution_KH_DIM_Length,ConfigInfo.ApplicationTensor_A.Convolution_KW_DIM_Length,ConfigInfo.ApplicationTensor_A.dataType,ConfigInfo.Convolution_Current_OH_Index,ConfigInfo.Convolution_Current_OW_Index,ConfigInfo.Convolution_Current_KH_Index,ConfigInfo.Convolution_Current_KW_Index)
      }
    }
  }

  //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
  //如果是memoryload_state === s_load_working，那么我们就要开始取数
  //如果是memoryload_state === s_load_end，那么我们就要结束取数
  val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_K)).W)) //总共要加载的张量大小，总加载的数据量不会超过Tensor_M*Tensor_K*ruduceWidthByte，这个是不会变的
  val CurrentLoaded_BlockTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val CurrentLoaded_BlockTensor_K = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

  //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
  //用sourceid做索引，存储Scarchpad的地址和bank号，是一组寄存器

  val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new ASourceIdSearch).getWidth.W))))

  val MaxBlockTensor_M_Index = ScaratchpadTensor_M
  val MaxBlockTensor_K_Index = ScaratchpadTensor_K

  val Init_Current_M_BaseAddr = RegInit(0.U(log2Ceil(MMUAddrWidth).W))//当前M不动的情况下的基地指


  val Request = io.LocalMMUIO.Request

  val Current_IH_Index = WireInit(0.S((log2Ceil(ConvolutionDIM_Max)+1).W))
  //TODO:这里可能是性能瓶颈，如果这里不满足时序要求，我们可以在这里切流水，提前算好然后喂进AML,我们有一百种方法在这里优化时序:p
  //    Current_IH_Index := Cat(0.U(1.W),Convolution_Current_OH_Index).asSInt * Cat(0.U(1.W),Convolution_Stride_H).asSInt + Cat(0.U(1.W),Convolution_Current_KH_Index).asSInt - Cat(0.U(1.W),Convolution_KH_DIM_Length/2.U).asSInt
  Current_IH_Index := Cat(0.U(1.W),Convolution_Current_OH_Index).asSInt * Cat(0.U(1.W),Convolution_Stride_H).asSInt + Cat(0.U(1.W),Convolution_Current_KH_Index).asSInt - Cat(0.U(1.W),Convolution_KH_DIM_Length/2.U).asSInt//这里是计算当前的IH的index的初始值,如果变瓶颈了再改
  Current_IW_Index := Cat(0.U(1.W),Convolution_Current_OW_Index).asSInt * Cat(0.U(1.W),Convolution_Stride_W).asSInt + Cat(0.U(1.W),Convolution_Current_KW_Index).asSInt - Cat(0.U(1.W),Convolution_KW_DIM_Length/2.U).asSInt//这里是计算当前的IW的index的初始值,如果变瓶颈了再改
  val Current_IW_Index = WireInit(0.S((log2Ceil(ConvolutionDIM_Max)+1).W))
  val Current_IH_Index_U = Current_IH_Index(log2Ceil(ConvolutionDIM_Max)-1,0)
  val Current_IW_Index_U = Current_IW_Index(log2Ceil(ConvolutionDIM_Max)-1,0)
  val Is_invalid_IH_IW = Current_IH_Index < 0.S || Current_IW_Index < 0.S || Current_IH_Index >= Cat(0.U(1.W),Convolution_IH_DIM_Length).asSInt || Current_IW_Index >= Cat(0.U(1.W),Convolution_IW_DIM_Length).asSInt


  val Current_M_BaseAddr = RegInit(0.U(log2Ceil(MMUAddrWidth).W))//当前M不动的情况下的基地指,为了保证时序，我们需要提前算好
  //Current_M_BaseAddr这个值，需要根据下一次的IH和IW确定
  //IH和IW是否超界，如果超界，需要进行0填充，由Is_invalid_IH_IW根据OW、OH、stride_H、stride_W、KH、KW来判断
  //Current_M_BaseAddr只要在IH和IW不越界的情况下，值是对的即可。
  //OW达到上限变为0时，OH+1，OW=0，此时IW = kh - kh_dim/2，这个值不能写错！此时IW对application_A的地址偏移贡献为(kh - kh_dim/2)*iw_stride,此时IH对application_A的地址偏移贡献为(oh+1)*Convolution_Stride_H*ih_stride


  Request.valid := false.B
  when(memoryload_state === s_load_init){
    memoryload_state := s_load_working
    TotalLoadSize := 0.U
    CurrentLoaded_BlockTensor_M := 0.U
    CurrentLoaded_BlockTensor_K := 0.U
    Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Current_IW_Index_U + Tensor_A_BaseVaddr
    Init_Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * Current_IW_Index_U + Tensor_A_BaseVaddr

  }.elsewhen(memoryload_state === s_load_working){
    //根据不同的MemoryOrder，执行不同的访存模式

    //Is_invalid_IH_IW 需要单独处理，找机会和某一次写回合并！根据此刻某个bank是否被占用来偷时点

    //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
    //不用担心乘法电路延迟，EDA工具会把他优化好，再不济也能提前算好，我们有一万种优化时序的方法:)
    Request.bits.RequestVirtualAddr := Current_M_BaseAddr + CurrentLoaded_BlockTensor_K * ReduceWidthByte.U

    val sourceId = Mux(Conherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
    Request.bits.RequestConherent := Conherent
    Request.bits.RequestSourceID := sourceId.bits
    Request.bits.RequestType_isWrite := false.B
    Request.valid := true.B
    when(CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index || CurrentLoaded_BlockTensor_K === MaxBlockTensor_K_Index || Is_invalid_IH_IW)//Is_invalid_IH_IW时，不发出访存请求，尝试直接0填充
    {
      Request.valid := false.B
    }

    //输出oh,ow,ih,iw,is_invalid
    if (YJPAMLDebugEnable)
    {
      printf("[AML<%d>]Current_OH_Index:%d,Current_OW_Index:%d,Current_IH_Index:%d,Current_IW_Index:%d,Is_invalid_IH_IW:%d\n",io.DebugInfo.DebugTimeStampe,Convolution_Current_OH_Index,Convolution_Current_OW_Index,Current_IH_Index_U,Current_IW_Index_U,Is_invalid_IH_IW)
    }
    //数据在Scarachpad中的编排
    //数据会先排M，再排K，
    //AVector一定是不同M的数据，K不断送入，直到K迭代完成，再换新的M，
    //这里的0 1 2 3是一个K连续的ReduceWidth宽的数据
    //   K 0 1 2 3 4 5 6 7     time     AVector     ScaratchpadData也这么排布
    // M                        0       0 8 g o             {bank[0] [1] [2] [3]}
    // 0   0 1 2 3 4 5 6 7      1       1 9 h p   |addr    0 |    0   8   g   o
    // 1   8 9 a b c d e f      2       2 a i q   |        1 |    1   9   h   p
    // 2   g h i j k l m n      3       3 b j r   |        2 |    2   a   i   q
    // 3   o p g r s t u v      4       4 c k s   |        3 |    3   b   j   r
    // 4   w x y z .......      5       5 d l t   |        4 |    4   c   k   s
    // 5   !..............      6       6 e m u   |        5 |    5   d   l   t
    // 6   @..............      7       7 f n v   |        6 |    6   e   m   u
    // 7   #..............      8       w ! @ #   |        7 |    7   f   n   v
    // 8   $..............      9       .......   | ...........................
    //
    //
    // 在内存中的排布则是 0 1 2 3 4 5 6 7 8 9 a b c d e f g h i j k l m n o p q r s t u v w x y z .......

    when(Request.fire && sourceId.valid && !Is_invalid_IH_IW){
      val TableItem = Wire(new ASourceIdSearch)
      TableItem.ScratchpadBankId := CurrentLoaded_BlockTensor_M % AScratchpadNBanks.U
      TableItem.ScratchpadAddr := ((CurrentLoaded_BlockTensor_M / AScratchpadNBanks.U) * Tensor_K.U) + CurrentLoaded_BlockTensor_K
      SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

      if(YJPAMLDebugEnable)
      {
        //输出当前的IH和IW，M，K，当前访存任务的虚地址，五个一起输出
        printf("[AML<%d>]Current_IH_Index:%d,Current_IW_Index:%d,CurrentLoaded_BlockTensor_M:%d,CurrentLoaded_BlockTensor_K:%d,RequestVirtualAddr:%x\n",io.DebugInfo.DebugTimeStampe,Current_IH_Index,Current_IW_Index,CurrentLoaded_BlockTensor_M,CurrentLoaded_BlockTensor_K,Request.bits.RequestVirtualAddr)
      }
      when(CurrentLoaded_BlockTensor_M < MaxBlockTensor_M_Index && CurrentLoaded_BlockTensor_K < MaxBlockTensor_K_Index){
        CurrentLoaded_BlockTensor_M := CurrentLoaded_BlockTensor_M + 1.U
        Convolution_Current_OW_Index := Convolution_Current_OW_Index + 1.U
        Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * (Current_IW_Index_U + Convolution_Stride_W) + Tensor_A_BaseVaddr //下一个M的地址,IW正常增加
        when(Convolution_Current_OW_Index === Convolution_OW_DIM_Length - 1.U)
        {
          Convolution_Current_OW_Index := 0.U //OW变成0了
          Convolution_Current_OH_Index := Convolution_Current_OH_Index + 1.U //OH+1
          //计算这个地址可能是性能瓶颈，如果这里不满足时序要求，我们可以切流水算好地址，然后喂进来。做成一个算地址的FIFO即可，单单多几拍算地址的延迟而已，这里求地址的吞吐不变
          Current_M_BaseAddr := IH_Stride * (Current_IH_Index_U + Convolution_Stride_H) + (Convolution_Current_KW_Index - Convolution_KW_DIM_Length/2.U) * IW_Stride + Tensor_A_BaseVaddr //下一个M的地址，IH正常增加，这要看kernel的值
        }
        when(CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index - 1.U)
        {
          CurrentLoaded_BlockTensor_M := 0.U
          CurrentLoaded_BlockTensor_K := CurrentLoaded_BlockTensor_K + 1.U
          Convolution_Current_OH_Index := Init_Convolution_Current_OH_Index
          Convolution_Current_OW_Index := Init_Convolution_Current_OW_Index
          Current_M_BaseAddr := Init_Current_M_BaseAddr
        }
      }
    }.elsewhen(Is_invalid_IH_IW)
    {
      val TableItem = Wire(new ASourceIdSearch)
      TableItem.ScratchpadBankId := CurrentLoaded_BlockTensor_M % AScratchpadNBanks.U
      TableItem.ScratchpadAddr := ((CurrentLoaded_BlockTensor_M / AScratchpadNBanks.U) * Tensor_K.U) + CurrentLoaded_BlockTensor_K
      TableItem.asUInt

      val Response_sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
      val Response_ScratchpadBankId = SoureceIdSearchTable(Response_sourceId).asTypeOf(new ASourceIdSearch).ScratchpadBankId
      //有两种情况会发生NACK
      //1.如果当前的写回任务(优先级更高)与当前的0填充任务，是同一个Bank的任务，则进行NACK处理
      //2.前面的0填充任务还没有被处理，那么这个任务就需要NACK，如果连续发生NACK，就需要Stall
      val IsNACK_From_Response = (io.LocalMMUIO.Response.valid && (Response_ScratchpadBankId === TableItem .ScratchpadBankId))
      val IsNACK_From_NACKReg = NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId)
      val IsNACK = IsNACK_From_Response || IsNACK_From_NACKReg

      val NACK2Stall = WireInit(false.B)//是否需要stall,如果当前的bank有NACK，那么就需要stall，暂时停止译码(基本不可能发生)

      for (i <- 0 until AScratchpadNBanks)
      {
        //当前bank的NACK是有效的，且当前的bank不是当前Response的bank
        Zero_Fill_TableItem_Valid(i) := NACK_ZeroFill_Hloding_Valid(i) && ((io.LocalMMUIO.Response.valid === false.B)||(NACK_ZeroFill_Hloding_Reg(i).asTypeOf(new ASourceIdSearch).ScratchpadBankId =/= TableItem.ScratchpadBankId))
        //如果Zero_Fill_TableItem_Valid为真，则NACK_ZeroFill_Hloding一定会被处理，所以这里要清空
        NACK_ZeroFill_Hloding_Valid(i) := Mux(Zero_Fill_TableItem_Valid(i),false.B,NACK_ZeroFill_Hloding_Valid(i))
        Zero_Fill_TableItem(i) := Mux(Zero_Fill_TableItem_Valid(i),NACK_ZeroFill_Hloding_Reg(i),0.U.asTypeOf(new ASourceIdSearch))
      }

      when(IsNACK_From_Response && !IsNACK_From_NACKReg){
        //由于与Response的bank冲突，那么这个任务此刻不能被写回，且Reg中没有NACK，那么就需要将这个任务放入NACK的Reg中
        NACK_ZeroFill_Hloding_Reg(TableItem.ScratchpadBankId) := TableItem
        NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId) := true.B
      }.elsewhen(IsNACK_From_Response && IsNACK_From_NACKReg){
        //如果当前任务的bank已经有NACK了，那就看这个NACK这一周期是否被处理了，如果没有被处理，那么就需要stall
        when(Zero_Fill_TableItem_Valid(TableItem.ScratchpadBankId))
        {
          NACK_ZeroFill_Hloding_Reg(TableItem.ScratchpadBankId) := TableItem
          NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId) := true.B
        }.otherwise
        {
          NACK2Stall := true.B
        }
      }.elsewhen(!IsNACK_From_Response && IsNACK_From_NACKReg)
      {
        //如果当前任务的Response bank没有冲突，但是Reg中有NACK冲突了，那么就看这个NACK这一周期是否被处理了，如果没有被处理，那么就需要stall
        when(Zero_Fill_TableItem_Valid(TableItem.ScratchpadBankId))
        {
          NACK_ZeroFill_Hloding_Reg(TableItem.ScratchpadBankId) := TableItem
          NACK_ZeroFill_Hloding_Valid(TableItem.ScratchpadBankId) := true.B
        }.otherwise
        {
          //讲道理这个情况不可能发生
          NACK2Stall := true.B
        }
      }.otherwise
      {
        //恭喜我们没有发生NACK,这是常见情况
        Zero_Fill_TableItem_Valid(TableItem.ScratchpadBankId) := true.B
        Zero_Fill_TableItem(TableItem.ScratchpadBankId) := TableItem
      }


      when(!NACK2Stall)//可以继续译码
      {
        when(CurrentLoaded_BlockTensor_M < MaxBlockTensor_M_Index && CurrentLoaded_BlockTensor_K < MaxBlockTensor_K_Index){
          CurrentLoaded_BlockTensor_M := CurrentLoaded_BlockTensor_M + 1.U
          Convolution_Current_OW_Index := Convolution_Current_OW_Index + 1.U
          Current_M_BaseAddr := IH_Stride * Current_IH_Index_U + IW_Stride * (Current_IW_Index_U + Convolution_Stride_W) + Tensor_A_BaseVaddr //下一个M的地址,IW正常增加
          when(Convolution_Current_OW_Index === Convolution_OW_DIM_Length - 1.U)
          {
            Convolution_Current_OW_Index := 0.U //OW变成0了
            Convolution_Current_OH_Index := Convolution_Current_OH_Index + 1.U //OH+1
            Current_M_BaseAddr := IH_Stride * (Current_IH_Index_U + Convolution_Stride_H) + (Convolution_Current_KW_Index - Convolution_KW_DIM_Length/2.U) * IW_Stride + Tensor_A_BaseVaddr //下一个M的地址，IH正常增加，这要看kernel的值
          }
          when(CurrentLoaded_BlockTensor_M === MaxBlockTensor_M_Index - 1.U)
          {
            CurrentLoaded_BlockTensor_M := 0.U
            CurrentLoaded_BlockTensor_K := CurrentLoaded_BlockTensor_K + 1.U
            Convolution_Current_OH_Index := Init_Convolution_Current_OH_Index
            Convolution_Current_OW_Index := Init_Convolution_Current_OW_Index
            Current_M_BaseAddr := Init_Current_M_BaseAddr
          }
        }
      }
    }
    //接受访存的返回值
    //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
    //根据response的sourceid，找到对应的Scarchpad的地址和bank号，回填数据
    when(io.LocalMMUIO.Response.valid){
      //Trick注意这个设计，是doublebuffer的，AB只能是doublebuffer，回数一定是不会堵的，而且我们有时间对数据进行压缩解压缩～
      //如果要做release设计，要么数据位宽翻倍，腾出周期来使得有空泡能给写任务进行，要么就是数据位宽不变，将读写端口变成独立的读和独立的写端口
      val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
      val ScratchpadBankId = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).ScratchpadBankId
      val ScratchpadAddr = SoureceIdSearchTable(sourceId).asTypeOf(new ASourceIdSearch).ScratchpadAddr
      val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
      //Scartchpad->MemoryLoader->MMU->Memory Bus->Memory上的长组合逻辑链，可以实现一下，为后续的开发做准备
      //否则就靠软件来保证数据流和访存流，保证访存流的稳定性，一定不会堵，就可以省下这个长组合逻辑的延迟？
      //还有一点，我们的ScartchPad是写优先的呀！！！所以只要写端口数唯一，就不会堵，不需要fifo～～～
      //Trick:写优先是真的很有说法，本来外部存储就是慢的，读快速存储器的任务等一等就好了，但是所有的ScartchPad都想要读数据的，不能等，所以写优先

      //根据response的的id
      io.ToScarchPadIO.BankAddr.bits := ScratchpadAddr
      io.ToScarchPadIO.BankId.bits := ScratchpadBankId
      io.ToScarchPadIO.Data.bits := ResponseData
      io.ToScarchPadIO.BankAddr.valid := true.B
      io.ToScarchPadIO.BankId.valid := true.B
      io.ToScarchPadIO.Data.valid := true.B
      if (YJPAMLDebugEnable)
      {
        //输出这次response的信息
        printf("[AML]ResponseData:%x,ScratchpadBankId:%d,ScratchpadAddr:%d\n",ResponseData,ScratchpadBankId,ScratchpadAddr)
        //response的sourceid
        printf("[AML]ResponseSourceID:%d\n",sourceId)
        //输出这次的totalload
        printf("[AML]TotalLoadSize:%d\n",TotalLoadSize)
      }
    }

    //完成零填充写回
    for (i <- 0 until AScratchpadNBanks)
    {
      when(Zero_Fill_TableItem_Valid(i))
      {
        io.ToScarchPadIO.BankAddr.bits := Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadAddr
        io.ToScarchPadIO.BankId.bits := Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadBankId
        io.ToScarchPadIO.Data.bits := 0.U
        io.ToScarchPadIO.BankAddr.valid := true.B
        io.ToScarchPadIO.BankId.valid := true.B
        io.ToScarchPadIO.Data.valid := true.B
        if(YJPAMLDebugEnable)
        {
          //输出这次的零填充信息
          printf("[AML]ZeroFillData:%x,ScratchpadBankId:%d,ScratchpadAddr:%d\n",0.U,Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadBankId,Zero_Fill_TableItem(i).asTypeOf(new ASourceIdSearch).ScratchpadAddr)
        }
      }
    }

    TotalLoadSize := TotalLoadSize + io.LocalMMUIO.Response.valid.asUInt + Zero_Fill_TableItem_Valid.map(_.asUInt).reduce(_+_)
    when(TotalLoadSize === MaxBlockTensor_M_Index * MaxBlockTensor_K_Index){
      memoryload_state := s_load_end
    }
  }.elsewhen(memoryload_state === s_load_end){
    ConfigInfo.MicroTaskEndValid := true.B
    when(ConfigInfo.MicroTaskEndValid && ConfigInfo.MicroTaskEndReady){
      memoryload_state := s_load_idle
      state := s_idle
      if(YJPAMLDebugEnable)
      {
        printf("[AML<%d>]AMemoryLoader Task End\n",io.DebugInfo.DebugTimeStampe)
      }
    }
  }.otherwise{

  }

}