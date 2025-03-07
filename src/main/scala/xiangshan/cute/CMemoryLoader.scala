
package xiangshan.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._
//import boom.util._
import freechips.rocketchip.util.SeqToAugmentedSeq

//CMemoryLoader，用于加载C矩阵的数据，供给Scratchpad使用
//从不同的存储介质中加载数据，供给Scratchpad使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据Scarchpad的设计，完成Tensor的切分以及将数据的填入Scaratchpad

class CSourceIdSearch extends Bundle with HWParameters{
  val ScratchpadBankId =UInt(log2Ceil(CScratchpadNBanks).W)
  val ScratchpadAddr = UInt(log2Ceil(CScratchpadBankNEntrys).W)
}

class CMemoryLoader(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new Bundle{
    val ToScarchPadIO = Flipped(new CMemoryLoaderScaratchpadIO)
    val ConfigInfo = Flipped(new CMLMicroTaskConfigIO)
    val LocalMMUIO = Flipped(new LocalMMUIO)
    val DebugInfo = Input(new DebugInfoIO)
  })

  io.ConfigInfo.MicroTaskEndValid := false.B
  io.ConfigInfo.MicroTaskReady := false.B
  io.ToScarchPadIO.ReadRequestToScarchPad.BankAddr := 0.U.asTypeOf(io.ToScarchPadIO.ReadRequestToScarchPad.BankAddr)
  io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr := 0.U.asTypeOf(io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr)
  io.ToScarchPadIO.WriteRequestToScarchPad.Data := 0.U.asTypeOf(io.ToScarchPadIO.WriteRequestToScarchPad.Data)
  // io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr.bits := 0.U
  // io.ToScarchPadIO.WriteRequestToScarchPad.Data.bits := 0.U.asTypeOf(io.ToScarchPadIO.WriteRequestToScarchPad.Data.bits)
  io.LocalMMUIO.Request.bits.RequestConherent := false.B
  io.LocalMMUIO.Request.bits.RequestData := 0.U
  io.LocalMMUIO.Request.bits.RequestSourceID := 0.U
  io.LocalMMUIO.Request.bits.RequestType_isWrite := false.B
  io.LocalMMUIO.Request.bits.RequestVirtualAddr := false.B
  io.LocalMMUIO.Response.ready := false.B


  val ConfigInfo = io.ConfigInfo

  val ScaratchpadTensor_M = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val ScaratchpadTensor_N = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

  val Tensor_C_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))
  val Tensor_D_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))


  //任务状态机 先来个简单的，顺序读取所有分块矩阵
  val s_idle :: s_mm_task :: s_write :: Nil = Enum(3)
  val state = RegInit(s_idle)

  //访存读状态机，用来配合流水线刷新
  val s_load_idle :: s_load_init :: s_load_working :: s_load_end :: Nil = Enum(4)
  val memoryload_state = RegInit(s_load_idle)
  val MemoryOrder_LoadConfig = RegInit(MemoryOrderType.OrderTypeUndef)

  //访存写状态机，用来配合流水线刷新
  val s_store_idle :: s_store_init :: s_store_working :: s_store_end :: Nil = Enum(4)
  val memorystore_state = RegInit(s_store_idle)

  val Tensor_Block_BaseAddr = Reg(UInt(MMUAddrWidth.W)) //分块矩阵的基地址

  val IsConherent = RegInit(true.B) //是否一致性访存的标志位，由TaskController提供
  val Is_Transpose = RegInit(false.B) //是否转置的标志位，由TaskController提供

  val HasScarhpadRead = WireInit(false.B)
  val HasScarhpadWrite = WireInit(false.B)
  io.ToScarchPadIO.ReadWriteRequest := Cat(HasScarhpadRead,Cat(HasScarhpadWrite,Cat(0.U(1.W),0.U(1.W))))

  val ApplicationTensor_C_Stride_M = RegInit(0.U(MMUAddrWidth.W))
  val ApplicationTensor_D_Stride_M = RegInit(0.U(MMUAddrWidth.W))

  val Is_ZeroLoad = RegInit(false.B)
  val Is_FullLoad = RegInit(false.B)
  val Is_RepeatRowLoad = RegInit(false.B)

  val C_DataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))
  val D_DataType = RegInit(0.U(ElementDataType.DataTypeBitWidth.W))

  when(state === s_idle)
  {
    io.ConfigInfo.MicroTaskReady := true.B
    //如果configinfo有效
    when(io.ConfigInfo.MicroTaskReady && io.ConfigInfo.MicroTaskValid){
      state := s_mm_task
      when(io.ConfigInfo.IsLoadMicroTask === true.B && io.ConfigInfo.IsStoreMicroTask === false.B){
        memoryload_state := s_load_init
        Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr
        ApplicationTensor_C_Stride_M := io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M
        IsConherent := io.ConfigInfo.Conherent

        Is_ZeroLoad := io.ConfigInfo.LoadTaskInfo.Is_ZeroLoad
        Is_FullLoad := io.ConfigInfo.LoadTaskInfo.Is_FullLoad
        Is_RepeatRowLoad := io.ConfigInfo.LoadTaskInfo.Is_RepeatRowLoad

        C_DataType := io.ConfigInfo.ApplicationTensor_C.dataType
        if(YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]Load C Tensor Start, Tensor_Block_BaseAddr: %x, ApplicationTensor_C_Stride_M: %x, IsConherent: %x\n", io.DebugInfo.DebugTimeStampe, io.ConfigInfo.ApplicationTensor_C.BlockTensor_C_BaseVaddr, io.ConfigInfo.ApplicationTensor_C.ApplicationTensor_C_Stride_M, io.ConfigInfo.Conherent)
        }

      }.elsewhen(io.ConfigInfo.IsLoadMicroTask === false.B && io.ConfigInfo.IsStoreMicroTask === true.B){
        memorystore_state := s_store_init
        Tensor_Block_BaseAddr := io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr
        IsConherent := io.ConfigInfo.Conherent
        ApplicationTensor_D_Stride_M := io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M
        Is_Transpose := io.ConfigInfo.Is_Transpose

        D_DataType := io.ConfigInfo.ApplicationTensor_D.dataType
        if(YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Start<%d>]Store D Tensor Start, Tensor_Block_BaseAddr: %x, ApplicationTensor_D_Stride_M: %x, IsConherent: %x, Is_Transpose: %x\n", io.DebugInfo.DebugTimeStampe, io.ConfigInfo.ApplicationTensor_D.BlockTensor_D_BaseVaddr, io.ConfigInfo.ApplicationTensor_D.ApplicationTensor_D_Stride_M, io.ConfigInfo.Conherent, io.ConfigInfo.Is_Transpose)
        }

      }.otherwise{
        //闲闲没事做
      }
      ScaratchpadTensor_M := io.ConfigInfo.ScaratchpadTensor_M
      ScaratchpadTensor_N := io.ConfigInfo.ScaratchpadTensor_N

    }
  }



  //三个张量的虚拟地址，肯定得是连续的，这个可以交给操作系统和编译器来保证

  //C的数据需要在这里完成reorder，然后写入memory。
  //同时也能从memory中读取数据，然后reorder，然后写入Scartchpad


  //这里的Scaratchpad，有可以节省大小的方案，就是尽可能早的去标记某个数据是无效的，然后对下一个数据发出请求，这样对SRAM的读写端口数量要求就高了，多读写端口vsdoublebufferSRAM
  //LLC的访存带宽我们设定成和每个bank的每个entry的大小一样。

  //处理取数逻辑，AScartchpad的数据大概率是LLC内的数据，所以我们可以直接从LLC中取数
  //如果是memoryload_state === s_load_init，那么我们就要初始化各个寄存器
  //如果是memoryload_state === s_load_working，那么我们就要开始取数
  //如果是memoryload_state === s_load_end，那么我们就要结束取数
  val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共要执行的SCP的写入的数据量
  val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总发出的Memory请求的数据量
  val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val CurrentLoaded_BlockTensor_N_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

  //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
  //用sourceid做索引，存储Scarchpad的地址和bank号，是一组寄存器

  // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new CSourceIdSearch)})
  val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new CSourceIdSearch).getWidth.W))))

  val MaxRequestIter = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W))

  //读数的FIFO
  //SCP_Fill_FIFO是用来记录数据的fifo，最多能暂存CMemoryLoaderReadFromMemoryFIFODepth个数据
  //只要有空位，就可以继续填数据，每次填数据需要完成如下操作
  //1.将数据填入fifo，更新Tail
  //2.将SCP_Fill_FIFO_Time置为0
  //3.将这个数据将要填入的scp的第一个地址填入SCP_Fill_FIFO_SCP_Addr
  //4.根据这个数据的对应的bank号(x)，将这个数据对应的Fill_FIFO的index填入Bank_Fill_Search_FIFO(x)(Bank_Fill_Search_FIFO_Head(x))
  //5.更新Bank_Fill_Search_FIFO_Head(x)
  //每次回填SCP需要完成如下操作：
  //1.查看每一个bank是否有数据需要回填
  //2.给每个准备回填数据的bank，找到其对应的Fill_FIFO的index，在这个fill_fifo[index]的filltime+1，如果filltime==MAX_Fill_Times，那么这个数据就用完了
  //3.更新FIFO，更新Tail，更新Table
  val SCP_Fill_Table = RegInit((VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U(LLCDataWidth.W)))))
  val SCP_Fill_Table_SCP_Addr = RegInit((VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(CScratchpadBankNEntrys).W)))))//记录这个LLC回的数是在scp的哪个地址
  val SCP_Fill_Table_Time = RegInit((VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U((log2Ceil(LLCDataWidthByte/CScratchpadEntryByteSize)+1).W)))))//记录这个LLC回的数需要回填的次数，完成就可以将数据释放了
  val SCP_Fill_Table_Free = SCP_Fill_Table_Time.map(_ === 0.U)//记录这个FIFO能否能填数据
  val SCP_Fill_Table_Valid = SCP_Fill_Table_Time.map(_ =/= 0.U)//记录这个FIFO里的数据是否有效
  val SCP_Fill_Table_Insert_Index = PriorityEncoder(SCP_Fill_Table_Free)//返回第一个空位的index
  val SCP_Fill_Table_Valid_Index = PriorityEncoder(SCP_Fill_Table_Valid)//返回第一个有效的index,RepeatRowLoad需要用到
  val SCP_Fill_Table_Not_Full = SCP_Fill_Table_Free.reduce(_ || _)//这个FIFO是否还有空位
  val SCP_Fill_Table_Not_Empty = SCP_Fill_Table_Valid.reduce(_ || _)//这个FIFO是否还有数据,用于RepeatRowLoad
  val SCP_Fill_Table_Head = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个FIFO里的数据的头指针,用于RepeatRowLoad
  val SCP_Fill_Table_Tail = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个FIFO里的数据的尾指针,用于RepeatRowLoad
  val MAX_Fill_Times = LLCDataWidthByte/CScratchpadEntryByteSize

  val Repeat_Fill_Is_Working = RegInit(false.B)//是否在回填数据
  val Repeat_Fill_Times = RegInit(0.U(log2Ceil(Tensor_M).W))//记录这个数据需要回填的次数
  val Repeat_Fill_Group_Times = RegInit(0.U(log2Ceil(LLCDataWidthByte/CScratchpadEntryByteSize).W))//记录这个数据需要回填的次数
  val Repeat_Fill_Table_Index = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个数据在FIFO里的index
  val Repeat_Fill_Request_Infight = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))//记录这个有多少请求已经发出，由于我们一个发出的请求需要回填16拍，所以必须记录一下infight的数量，不能多发请求

  val Bank_Fill_Search_FIFO = RegInit((VecInit(Seq.fill(CScratchpadNBanks)(VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)))))))//记录fifo里的数据是哪个bank的
  val Bank_Fill_Search_FIFO_Head = RegInit((VecInit(Seq.fill(CScratchpadNBanks)(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)))))//想要往scp里bank(x)写的最后一个scp_fill_fifo的index
  val Bank_Fill_Search_FIFO_Tail = RegInit((VecInit(Seq.fill(CScratchpadNBanks)(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)))))
  val Bank_Fill_Search_FIFO_Full = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(false.B)))
  val Bank_Fill_Search_FIFO_Empty = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(true.B)))
  val Bank_Fill_Valid = Bank_Fill_Search_FIFO_Head.zip(Bank_Fill_Search_FIFO_Tail).map{case (h,t) => h =/= t}//每个bank，是否有数据需要写scp
  val Have_Bank_Fill = Bank_Fill_Valid.reduce(_ || _)//是否有数据需要写scp

  for(i <- 0 until CScratchpadNBanks){
    Bank_Fill_Search_FIFO_Full(i) := Bank_Fill_Search_FIFO_Tail(i) === WrapInc(Bank_Fill_Search_FIFO_Head(i), CMemoryLoaderReadFromMemoryFIFODepth)//fifo满了
    Bank_Fill_Search_FIFO_Empty(i) := Bank_Fill_Search_FIFO_Head(i) === Bank_Fill_Search_FIFO_Tail(i)//这个bank不需要写scp
  }

  val Request_M_Iter_Time = RegInit(0.U(log2Ceil(Matrix_M).W))
  // val Fill_N_Iter_Time = RegInit(0.U(log2Ceil(Tensor_N).W))
  //读数请求
  val ReadRequest = io.LocalMMUIO.Request
  ReadRequest.valid := false.B
  when(memoryload_state === s_load_init){
    memoryload_state := s_load_working
    TotalLoadSize := 0.U
    TotalRequestSize := 0.U
    CurrentLoaded_BlockTensor_M_Iter := 0.U
    CurrentLoaded_BlockTensor_N_Iter := 0.U
    MaxRequestIter := ScaratchpadTensor_M * ScaratchpadTensor_N * ResultWidthByte.U / (LLCDataWidthByte.U) //总共要发出的访存请求的次数
    Bank_Fill_Search_FIFO := 0.U.asTypeOf(Bank_Fill_Search_FIFO)
    Bank_Fill_Search_FIFO_Head := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Head)
    Bank_Fill_Search_FIFO_Tail := 0.U.asTypeOf(Bank_Fill_Search_FIFO_Tail)
    SCP_Fill_Table := 0.U.asTypeOf(SCP_Fill_Table)
    SCP_Fill_Table_SCP_Addr := 0.U.asTypeOf(SCP_Fill_Table_SCP_Addr)
    SCP_Fill_Table_Time := 0.U.asTypeOf(SCP_Fill_Table_Time)
    Request_M_Iter_Time := 0.U
    SCP_Fill_Table_Head := 0.U
    SCP_Fill_Table_Tail := 0.U
    Repeat_Fill_Times := 0.U
    Repeat_Fill_Group_Times := 0.U
    Repeat_Fill_Request_Infight := 0.U
    Repeat_Fill_Is_Working := false.B
  }.elsewhen(memoryload_state === s_load_working){
    //根据不同的MemoryOrder，执行不同的访存模式
    //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
    //担心乘法电路延迟，可以提前几个周期将乘法结果算好
    //TODO:注意这里的分块逻辑/地址拼接的逻辑，我们在设计MemoryOrderType分块的逻辑时，要考虑到这里的求地址的电路逻辑，是可以减少这部分的乘法电路的逻辑的
    //注意ScaratchPad内的存数的状态

    //数据在CScarachpad中的编排
    //数据会先排N，再排M,这里每个都是4byte的数据，是一个全精度的数据，是一个element，和AML、BML里的不是一个概念
    //   N 0 1 2 3 4 5 6 7     CScaratchpadData里的排布
    // M                               {bank  [0] [1]     [2] [3] }
    // 0   0 1 2 3 4 5 6 7   |addr    0 |    0123 89ab   ghij opgr
    // 1   8 9 a b c d e f   |        1 |    4567 cdef   klmn stuv
    // 2   g h i j k l m n   |        2 |    wxyz !...   @... #...
    // 3   o p g r s t u v   |        3 |    .... ....   .... ....
    // 4   w x y z .......   |        4 |    .... ....   .... ....
    // 5   !..............   |        5 |    .... ....   .... ....
    // 6   @..............   |        6 |    .... ....   .... ....
    // 7   #..............   |        7 |    .... ....   .... ....
    // 8   $..............   | ....................................

    //向量的访存顺序
    //01,89,gh,op,23,ab,ij,gr,45,cd,kl,st,67,ef,mn,uv,打散bank去填数据
    //   N 0 1 2 3 4 5 6 7     CScaratchpadData里的排布
    // M                               {bank  [0] [1]     [2] [3] }
    // 0   0 1 2 3 4 5 6 7   |addr    0 |      0   8       g   o
    // 1   8 9 a b c d e f   |        1 |      1   9       h   p
    // 2   g h i j k l m n   |        2 |      2   a       i   q
    // 3   o p g r s t u v   |        3 |    ...沙莉花园. ....   .... ....
    // 4   w x y z .......   |        4 |    .... ....   .... ....
    // 5   !..............   |        5 |    .... ....   .... ....
    // 6   @..............   |        6 |    .... ....   .... ....
    // 7   #..............   |        7 |    .... ....   .... ....
    // 8   $..............   | ....................................
    //

    when(Is_FullLoad)
    {
      val RequestScratchpadBankId = TotalRequestSize % CScratchpadNBanks.U //访存请求落在哪个ScratchpadBank上
      val RequestScratchpadAddr = (TotalRequestSize / CScratchpadNBanks.U) * MAX_Fill_Times.U //该访存请求的第零号数据，落在哪个ScratchpadBank的哪个地址上

      ReadRequest.bits.RequestVirtualAddr := Tensor_Block_BaseAddr + (CurrentLoaded_BlockTensor_M_Iter + Request_M_Iter_Time) * ApplicationTensor_C_Stride_M + CurrentLoaded_BlockTensor_N_Iter * C_DataType

      // val CurrentBankID = RequestScratchpadBankId
      // val CurrentFIFOIndex = FromMemoryLoaderReadFIFOHead

      val sourceId = Mux(IsConherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)


      ReadRequest.bits.RequestConherent := IsConherent
      ReadRequest.bits.RequestSourceID := sourceId.bits
      ReadRequest.bits.RequestType_isWrite := false.B
      ReadRequest.valid := (TotalRequestSize < MaxRequestIter)

      //确定这个访存请求一定会发出
      when(ReadRequest.fire){
        val TableItem = Wire(new CSourceIdSearch)
        TableItem.ScratchpadBankId := RequestScratchpadBankId
        TableItem.ScratchpadAddr := RequestScratchpadAddr
        SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

        Request_M_Iter_Time := Request_M_Iter_Time + 1.U//连续的跨bank去访存
        when(Request_M_Iter_Time === (Matrix_M - 1).U){
          Request_M_Iter_Time := 0.U
          CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + LLCDataWidthByte.U / C_DataType
          when(CurrentLoaded_BlockTensor_N_Iter + LLCDataWidthByte.U / C_DataType === ScaratchpadTensor_N){
            CurrentLoaded_BlockTensor_N_Iter := 0.U
            CurrentLoaded_BlockTensor_M_Iter := CurrentLoaded_BlockTensor_M_Iter + Matrix_M.U
          }
        }
        //输出发出的访存请求
        // printf("[CMemoryLoader]RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x\n", ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite)
        //输出这次请求的TableItem
        // printf("[CMemoryLoader]TableItem: %x, %x, %x\n", TableItem.ScratchpadBankId, TableItem.ScratchpadAddr, TableItem.FIFOIndex)

        //只要这条取数指令可以被发出，就计算下一个访存请求的地址
        //TODO:这里数据读取量定死了，需要为了支持边界情况，改一改
        //不过我们保证了数据是256bit对齐的～剩下的就是Tensor_M和Tensor_K不满足的情况思考好就行了
        //输出request的次数
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x, RequestTimes: %d\n", io.DebugInfo.DebugTimeStampe, ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite, TotalRequestSize)
        }
        when(TotalRequestSize === MaxRequestIter){
          //assert!
          //error!
        }.otherwise{
          TotalRequestSize := TotalRequestSize + 1.U
        }
      }

      val current_fill_fifo_full = WireInit(false.B)
      when(io.LocalMMUIO.Response.valid)
      {
        val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
        val ScratchpadBankId = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).ScratchpadBankId
        current_fill_fifo_full := Bank_Fill_Search_FIFO_Full(ScratchpadBankId)
      }

      io.LocalMMUIO.Response.ready := SCP_Fill_Table_Not_Full && (current_fill_fifo_full === false.B)
      //接受访存的返回值
      //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
      //根据response的sourceid，找到对应的Scarchpad的地址和bank号，回填数据
      when(io.LocalMMUIO.Response.fire){
        val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
        val ScratchpadBankId = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).ScratchpadBankId
        val ScratchpadAddr = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).ScratchpadAddr
        val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
        val FIFOIndex = Bank_Fill_Search_FIFO_Head(ScratchpadBankId)//该bank的fill_fifo_index，标注了它当前在fillfifo的哪个位置，我们一共有bank个fill_fifo

        SCP_Fill_Table(SCP_Fill_Table_Insert_Index) := ResponseData
        SCP_Fill_Table_SCP_Addr(SCP_Fill_Table_Insert_Index) := ScratchpadAddr
        SCP_Fill_Table_Time(SCP_Fill_Table_Insert_Index) := MAX_Fill_Times.U

        Bank_Fill_Search_FIFO(ScratchpadBankId)(FIFOIndex) := SCP_Fill_Table_Insert_Index
        Bank_Fill_Search_FIFO_Head(ScratchpadBankId) := WrapInc(Bank_Fill_Search_FIFO_Head(ScratchpadBankId), CMemoryLoaderReadFromMemoryFIFODepth)

        //输出回填的数据
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]ResponseData: %x, ScratchpadBankId: %x, ScratchpadAddr: %x, FIFOIndex: %x\n",io.DebugInfo.DebugTimeStampe, ResponseData, ScratchpadBankId, ScratchpadAddr, FIFOIndex)
        }
      }

      //检查每个bank是否有数据需要回填
      HasScarhpadWrite := Have_Bank_Fill
      val Current_Fill_SCP_Time = WireInit(VecInit(Seq.fill(CScratchpadNBanks)(0.U(1.W))))
      for (i <- 0 until CScratchpadNBanks){
        when(Bank_Fill_Search_FIFO_Empty(i) === false.B){
          val CurrentFIFOIndex = Bank_Fill_Search_FIFO(i)(Bank_Fill_Search_FIFO_Tail(i))
          when(io.ToScarchPadIO.ReadWriteResponse(ScaratchpadTaskType.WriteFromMemoryLoaderIndex) === true.B)
          {
            Current_Fill_SCP_Time(i) := 1.U
            val ScarchPadWriteRequest = io.ToScarchPadIO.WriteRequestToScarchPad
            val FIFOData = WireInit((VecInit(Seq.fill(MAX_Fill_Times)(0.U(CScratchpadEntryByteSize.W)))))
            FIFOData := SCP_Fill_Table(CurrentFIFOIndex).asTypeOf(FIFOData)
            ScarchPadWriteRequest.BankAddr(i).bits := SCP_Fill_Table_SCP_Addr(CurrentFIFOIndex) + (MAX_Fill_Times.U - SCP_Fill_Table_Time(CurrentFIFOIndex))
            ScarchPadWriteRequest.BankAddr(i).valid := true.B
            ScarchPadWriteRequest.Data(i).bits := FIFOData(MAX_Fill_Times.U - SCP_Fill_Table_Time(CurrentFIFOIndex))
            ScarchPadWriteRequest.Data(i).valid := true.B

            SCP_Fill_Table_Time(CurrentFIFOIndex) := SCP_Fill_Table_Time(CurrentFIFOIndex) - 1.U
            when(SCP_Fill_Table_Time(CurrentFIFOIndex) === 1.U){
              Bank_Fill_Search_FIFO_Tail(i) := WrapInc(Bank_Fill_Search_FIFO_Tail(i), CMemoryLoaderReadFromMemoryFIFODepth)
            }

            if (YJPCMLDebugEnable)
            {
              //输出fill_time 和 fifoindex
              printf("[CMemoryLoader_Load<%d>]bankid: %d,CurrentFIFOIndex %d,ScartchPadAddr: %x, SCP_Fill_Table_Time(CurrentFIFOIndex): %d\n", io.DebugInfo.DebugTimeStampe,i.U, CurrentFIFOIndex, SCP_Fill_Table_SCP_Addr(CurrentFIFOIndex), SCP_Fill_Table_Time(CurrentFIFOIndex))
              printf("[CMemoryLoader_Load<%d>]bankid: %d,ScartchPadAddr: %x, BankAddr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,i.U, SCP_Fill_Table_SCP_Addr(CurrentFIFOIndex), ScarchPadWriteRequest.BankAddr(i).bits, ScarchPadWriteRequest.Data(i).bits)
            }
          }.otherwise
          {
            if (YJPCMLDebugEnable)
            {
              printf("[CMemoryLoader_Load<%d>]bankid: %d no authority\n", io.DebugInfo.DebugTimeStampe,i.U)
            }
          }
        }
      }

      val Current_Load_Fill_Size = WireInit(0.U((log2Ceil(CScratchpadNBanks)+1).W))
      Current_Load_Fill_Size := PopCount(Current_Fill_SCP_Time.asUInt)

      TotalLoadSize := TotalLoadSize + Current_Load_Fill_Size

      if (YJPCMLDebugEnable)
      {
        when(Current_Load_Fill_Size =/= 0.U)
        {
          printf("[CMemoryLoader_Load<%d>]Current_Load_Fill_Size: %d, TotalLoadSize: %d, MaxLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, Current_Load_Fill_Size, TotalLoadSize, MaxRequestIter * MAX_Fill_Times.U)
        }
      }
      //状态机切换
      when(TotalLoadSize === (MaxRequestIter * MAX_Fill_Times.U)){
        memoryload_state := s_load_end
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]LoadEnd\n",io.DebugInfo.DebugTimeStampe)
        }
      }
    }.elsewhen(Is_ZeroLoad)
    {
      //给所有的bank发出写0的请求
      HasScarhpadWrite := true.B
      //每次写所有bank的一个entry，总共要写CScratchpadBankNEntrys次
      val Max_ZeroLoad_Write_Times = CScratchpadBankNEntrys
      for (i <- 0 until CScratchpadNBanks)
      {
        io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr(i).bits := TotalLoadSize
        io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr(i).valid := true.B
        io.ToScarchPadIO.WriteRequestToScarchPad.Data(i).bits := 0.U
        io.ToScarchPadIO.WriteRequestToScarchPad.Data(i).valid := true.B
      }

      when(io.ToScarchPadIO.ReadWriteResponse(ScaratchpadTaskType.WriteFromMemoryLoaderIndex) === true.B)
      {
        TotalLoadSize := TotalLoadSize + 1.U
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]ZeroLoad, TotalLoadSize: %d\n",io.DebugInfo.DebugTimeStampe, TotalLoadSize)
        }
        when(TotalLoadSize === (Max_ZeroLoad_Write_Times-1).U)
        {
          memoryload_state := s_load_end
          if (YJPCMLDebugEnable)
          {
            printf("[CMemoryLoader_Load<%d>]ZeroLoadEnd\n",io.DebugInfo.DebugTimeStampe)
          }
        }
      }

    }.elsewhen(Is_RepeatRowLoad)
    {
      //由于RepeatRowLoad的特殊性，我们一次Load需要写SCP很多次,导致我们的FIFO在被写满时，会导致长时间的TL无法握手。
      //故，我们针对这样的情况，我们需要为每一个发出的访存请求预留一个FIFO的空位，这样就可以保证TL握手成功，从而不浪费访存带宽，这样可能会导致整体延迟增加(但不会低到阻碍吞吐)，但我们的访存带宽利用率一定不会低
      //获取整个Row的数据，然后重复填充，Row的总数据量为Tensor_N*C_DataType
      val sourceId = Mux(IsConherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)
      val Max_RepeatRowLoad_Memory_Load_Times = Tensor_N.U * C_DataType / LLCDataWidthByte.U //总共要发出的访存请求的次数
      val Max_SCP_Write_Times = Tensor_M*Tensor_N*ResultWidthByte/CScratchpad_Total_Bandwidth //总共要写入SCP的次数
      ReadRequest.bits.RequestVirtualAddr := Tensor_Block_BaseAddr +  CurrentLoaded_BlockTensor_N_Iter * C_DataType
      ReadRequest.bits.RequestConherent := IsConherent
      ReadRequest.bits.RequestSourceID := sourceId.bits
      ReadRequest.bits.RequestType_isWrite := false.B
      ReadRequest.valid := (TotalRequestSize < Max_RepeatRowLoad_Memory_Load_Times) && (Repeat_Fill_Request_Infight < (CMemoryLoaderReadFromMemoryFIFODepth-1).U)


      val Per_Load_Fill_SCP_Times = (Tensor_M/Matrix_M) * (LLCDataWidthByte/CScratchpadEntryByteSize)
      val Per_Data_Repeat_Times = (Tensor_M/Matrix_M) //每组数据要重复写SCP这么多次
      val Per_Memory_Load_Have_Data_Write_Group = (LLCDataWidthByte/CScratchpadEntryByteSize)//每次Memory的load，有几组数据要写回
      val Per_Write_SCP_Addr_Add = (Tensor_N / Matrix_N).U //一组数据Per_Data_Repeat_Times迭代中，下一次写入的scp地址的增量

      // val Load_Time = CurrentLoaded_BlockTensor_N_Iter / (LLCDataWidthByte.U/C_DataType)

      //向量的访存顺序
      //01,23,45,67.....
      //   N 0 1 2 3 4 5 6 7     CScaratchpadData里的排布
      // M                               {bank  [0] [1]     [2] [3] }
      // 0   0 1 2 3 4 5 6 7   |addr    0 |      0   0       0   0
      //                       |        1 |      1   1       1   1
      //                       |        2 |      2   2       2   2
      //                       |        . |    .... ....   .... ....
      //                       |        . |    .... ....   .... ....
      //                       |       15 |     15   15     15   15
      //                       |       16 |      0   0       0   0
      //                       |       17 |    .... ....   .... ....
      //                       | ....................................
      //
      //SCP的写回顺序
      //0,16,32,48.....每次加Per_Write_SCP_Addr_Add，一共写Per_Data_Repeat_Times次
      //1,17,33,49.....每次加Per_Write_SCP_Addr_Add，一共写Per_Data_Repeat_Times次
      //一次Memory的load，有Per_Memory_Load_Have_Data_Write_Group组数据，每组数据写回Per_Data_Repeat_Times次

      //确定这个访存请求一定会发出
      when(ReadRequest.fire){

        val TableItem = Wire(new CSourceIdSearch)
        TableItem.ScratchpadBankId := 0.U
        TableItem.ScratchpadAddr := TotalRequestSize * Per_Memory_Load_Have_Data_Write_Group.U//这个数据的第一个数据，落在哪个ScratchpadBank的哪个地址上
        SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt

        CurrentLoaded_BlockTensor_N_Iter := CurrentLoaded_BlockTensor_N_Iter + LLCDataWidthByte.U / C_DataType
        Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight + 1.U
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]RepeatRowLoad, RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x\n", io.DebugInfo.DebugTimeStampe, ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite)
        }
        when(TotalRequestSize === Max_RepeatRowLoad_Memory_Load_Times){
          //assert!
          //error!
        }.otherwise{
          TotalRequestSize := TotalRequestSize + 1.U
        }
      }

      io.LocalMMUIO.Response.ready := true.B
      when(io.LocalMMUIO.Response.fire){
        val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
        val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
        val FIFOIndex = SCP_Fill_Table_Insert_Index
        SCP_Fill_Table(FIFOIndex) := ResponseData
        SCP_Fill_Table_SCP_Addr(FIFOIndex) := SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).ScratchpadAddr
        SCP_Fill_Table_Time(FIFOIndex) := 1.U//当valid用

        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]RepeatRowLoad, ResponseData: %x, FIFOIndex: %x\n", io.DebugInfo.DebugTimeStampe, ResponseData, FIFOIndex)
        }
      }


      when(SCP_Fill_Table_Not_Empty && Repeat_Fill_Is_Working === false.B)
      {
        Repeat_Fill_Is_Working := true.B
        Repeat_Fill_Table_Index := SCP_Fill_Table_Valid_Index
        Repeat_Fill_Times := 0.U
        Repeat_Fill_Group_Times := 0.U
      }

      when(Repeat_Fill_Is_Working)
      {
        val CurrentFIFOData = SCP_Fill_Table(Repeat_Fill_Table_Index)
        val All_Group_Data = WireInit((VecInit(Seq.fill(Per_Memory_Load_Have_Data_Write_Group)(0.U(CScratchpadEntryBitSize.W)))))
        All_Group_Data := CurrentFIFOData.asTypeOf(All_Group_Data)
        val SCP_Write_Addr = SCP_Fill_Table_SCP_Addr(Repeat_Fill_Table_Index) + Repeat_Fill_Times * Per_Write_SCP_Addr_Add + Repeat_Fill_Group_Times

        val ScarchPadWriteRequest = io.ToScarchPadIO.WriteRequestToScarchPad

        for (i <- 0 until CScratchpadNBanks)
        {
          ScarchPadWriteRequest.BankAddr(i).bits := SCP_Write_Addr
          ScarchPadWriteRequest.BankAddr(i).valid := true.B
          ScarchPadWriteRequest.Data(i).bits := All_Group_Data(Repeat_Fill_Group_Times)
          ScarchPadWriteRequest.Data(i).valid := true.B
        }

        HasScarhpadWrite := true.B

        when(io.ToScarchPadIO.ReadWriteResponse(ScaratchpadTaskType.WriteFromMemoryLoaderIndex) === true.B)
        {
          Repeat_Fill_Times := Repeat_Fill_Times + 1.U
          TotalLoadSize := TotalLoadSize + 1.U
          when(Repeat_Fill_Times === (Per_Data_Repeat_Times - 1).U)
          {
            Repeat_Fill_Times := 0.U
            Repeat_Fill_Group_Times := Repeat_Fill_Group_Times + 1.U
            when(Repeat_Fill_Group_Times === (Per_Memory_Load_Have_Data_Write_Group - 1).U)
            {
              Repeat_Fill_Group_Times := 0.U
              Repeat_Fill_Is_Working := false.B
              Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight - 1.U
              when(ReadRequest.fire)
              {
                Repeat_Fill_Request_Infight := Repeat_Fill_Request_Infight
              }
              SCP_Fill_Table_Time(Repeat_Fill_Table_Index) := 0.U
            }
          }
          if (YJPCMLDebugEnable)
          {
            printf("[CMemoryLoader_Load<%d>]RepeatRowLoad,Repeat_Fill_Times:  %d,TotalLoadSize:  %d,Repeat_Fill_Group_Times:  %d, SCP_Write_Addr: %x, Data: %x\n", io.DebugInfo.DebugTimeStampe,Repeat_Fill_Times,TotalLoadSize,Repeat_Fill_Group_Times, SCP_Write_Addr, All_Group_Data(Repeat_Fill_Group_Times))
          }
        }
      }

      when(TotalLoadSize === (Max_SCP_Write_Times).U)
      {
        memoryload_state := s_load_end
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Load<%d>]RepeatRowLoadEnd\n", io.DebugInfo.DebugTimeStampe)
        }
      }
    }.otherwise
    {
      //error!
      assert(false.B, "Error! Load Task Type Error!")
    }



  }.elsewhen(memoryload_state === s_load_end){
    io.ConfigInfo.MicroTaskEndValid := true.B
    when(io.ConfigInfo.MicroTaskEndReady && io.ConfigInfo.MicroTaskEndValid){
      memoryload_state := s_load_idle
      state := s_idle
      if (YJPCMLDebugEnable)
      {
        printf("[CMemoryLoader_Load<%d>]Load Finish\n",io.DebugInfo.DebugTimeStampe)
      }
    }
  }.otherwise{
    //闲闲没事做
  }


  //Store时，SCP的数据肯定是Reduce_Dim主序的，顺序取数即可
  //写数请求
  val TotalStoreSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共存储的数据量，这个参数表示已经对MMU发出的存储请求次数
  val TotalStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共读取的请求数据量，这个参数表示已经对ScartchPad发出的读请求次数
  val Max_Load_SCP_Time = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共要发对SCAP的访存次数
  val Max_Stroe_Memory_Time = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共要发对LLC的访存次数
  val CurrentStore_BlockTensor_Major_DIM_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val CurrentStore_BlockTensor_Reduce_DIM_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val Max_BlockTensor_Reduce_DIM = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val Max_BlockTensor_Major_DIM = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val Per_Store_ReduceDim_Iter = RegInit(0.U((log2Ceil(Tensor_M)).W))

  //CMemoryLoaderReadFromScratchpadFIFODepth深度的fifo
  val FromScratchpadReadFIFO = RegInit(VecInit(Seq.fill(CMemoryLoaderReadFromScratchpadFIFODepth)(0.U(CScratchpad_Total_Bandwidth_Bit.W))))
  val FromScratchpadReadFIFOHead = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromScratchpadFIFODepth).W))
  val FromScratchpadReadFIFOTail = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromScratchpadFIFODepth).W))
  val FromScratchpadReadFIFOFull = FromScratchpadReadFIFOTail === WrapInc(FromScratchpadReadFIFOHead, CMemoryLoaderReadFromScratchpadFIFODepth)
  val FromScratchpadReadFIFO_ISSUE_Full = FromScratchpadReadFIFOTail === WrapInc(WrapInc(FromScratchpadReadFIFOHead, CMemoryLoaderReadFromScratchpadFIFODepth),CMemoryLoaderReadFromScratchpadFIFODepth)
  val FromScratchpadReadFIFOEmpty = FromScratchpadReadFIFOHead === FromScratchpadReadFIFOTail

  val Per_SCP_Load_Write_Memory_Time = CScratchpad_Total_Bandwidth_Bit/LLCDataWidth
  val FireTimes = RegInit(0.U(log2Ceil(CScratchpadNBanks).W))

  when(memorystore_state === s_store_init){
    memorystore_state := s_store_working
    TotalStoreSize := 0.U
    TotalStoreRequestSize := 0.U
    CurrentStore_BlockTensor_Major_DIM_Iter := 0.U
    CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
    FromScratchpadReadFIFO := 0.U.asTypeOf(FromScratchpadReadFIFO)
    FromScratchpadReadFIFOHead := 0.U
    FromScratchpadReadFIFOTail := 0.U
    Max_Load_SCP_Time := ScaratchpadTensor_M * ScaratchpadTensor_N * D_DataType / CScratchpad_Total_Bandwidth.U//总共要发对SCAP的访存次数
    // Max_Stroe_Memory_Time := ScaratchpadTensor_M * ScaratchpadTensor_N * D_DataType / LLCDataWidthByte.U//总共要发对LLC的访存次数
    Per_Store_ReduceDim_Iter := LLCDataWidthByte.U / D_DataType
    FireTimes := 0.U
    Max_BlockTensor_Reduce_DIM := Mux(Is_Transpose, ScaratchpadTensor_M, ScaratchpadTensor_N)
    Max_BlockTensor_Major_DIM := Mux(Is_Transpose, ScaratchpadTensor_N, ScaratchpadTensor_M)

    if(YJPCMLDebugEnable)
    {
      printf("[CMemoryLoader_Store<%d>]Store D Tensor Start, Max_Load_SCP_Time: %x\n", io.DebugInfo.DebugTimeStampe, ScaratchpadTensor_M * ScaratchpadTensor_N * D_DataType / CScratchpad_Total_Bandwidth.U)
    }
  }.elsewhen(memorystore_state === s_store_working){
    //如果ScartchPad的仲裁结果允许我们读取数据
    HasScarhpadRead := !FromScratchpadReadFIFO_ISSUE_Full && !FromScratchpadReadFIFOFull && TotalStoreRequestSize < Max_Load_SCP_Time
    when(HasScarhpadRead){
      //根据ScartchPad的仲裁结果，我们可以读取数据了
      for (i <- 0 until CScratchpadNBanks){
        io.ToScarchPadIO.ReadRequestToScarchPad.BankAddr(i).bits := TotalStoreRequestSize
        io.ToScarchPadIO.ReadRequestToScarchPad.BankAddr(i).valid := true.B
      }
      when(io.ToScarchPadIO.ReadWriteResponse(ScaratchpadTaskType.ReadFromMemoryLoaderIndex)){
        TotalStoreRequestSize := TotalStoreRequestSize + 1.U
      }
      if (YJPCMLDebugEnable)
      {
        printf("[CMemoryLoader_Store<%d>]TotalStoreRequestSize: %x\n", io.DebugInfo.DebugTimeStampe, TotalStoreRequestSize)
      }
    }

    //只要ScaratchPad的数据读数有效，就可以将这个数置入fifo
    val ReadResponseData_Valid = io.ToScarchPadIO.ReadRequestToScarchPad.ReadResponseData.map(_.valid).reduce(_ && _)
    val ReadResponseData_Bits = io.ToScarchPadIO.ReadRequestToScarchPad.ReadResponseData.map(_.bits)
    when(ReadResponseData_Valid){
      FromScratchpadReadFIFO(FromScratchpadReadFIFOHead) := ReadResponseData_Bits.asUInt
      FromScratchpadReadFIFOHead := WrapInc(FromScratchpadReadFIFOHead, CMemoryLoaderReadFromScratchpadFIFODepth)

      if (YJPCMLDebugEnable)
      {
        printf("[CMemoryLoader_Store<%d>]FromScratchpadReadFIFOHead: %x,FromScratchpadReadFIFOTail: %x, data: %x\n", io.DebugInfo.DebugTimeStampe, FromScratchpadReadFIFOHead,FromScratchpadReadFIFOTail, ReadResponseData_Bits.asUInt)
      }
    }

    //只要fifo内的数据有效，就可以写入LLC
    val WriteRequest = io.LocalMMUIO.Request
    WriteRequest.valid := false.B
    when(!FromScratchpadReadFIFOEmpty){

      val Request = List.fill(MAX_Fill_Times){Wire(new Bundle{
        val RequestVirtualAddr = UInt(MMUAddrWidth.W)
        val RequestConherent = Bool()
        val RequestData = UInt(MMUDataWidth.W)
        val RequestSourceID = UInt(SoureceMaxNumBitSize.W)
        val RequestType_isWrite = UInt(2.W) //0-读，1-写
      })}

      for(i <- 0 until MAX_Fill_Times){
        Request(i).RequestVirtualAddr := Tensor_Block_BaseAddr + (CurrentStore_BlockTensor_Major_DIM_Iter) * ApplicationTensor_D_Stride_M + CurrentStore_BlockTensor_Reduce_DIM_Iter * D_DataType
        Request(i).RequestConherent := IsConherent
        Request(i).RequestSourceID := 0.U //无所谓，反正是写入
        Request(i).RequestType_isWrite := true.B
        Request(i).RequestData := FromScratchpadReadFIFO(FromScratchpadReadFIFOTail)(i)
      }

      val SelectRequest = UIntToOH(FireTimes,MAX_Fill_Times)
      WriteRequest.bits.RequestVirtualAddr := PriorityMux(SelectRequest,Request.map(_.RequestVirtualAddr))
      WriteRequest.bits.RequestConherent := PriorityMux(SelectRequest,Request.map(_.RequestConherent))
      WriteRequest.bits.RequestSourceID := io.LocalMMUIO.ConherentRequsetSourceID.bits
      WriteRequest.bits.RequestType_isWrite := PriorityMux(SelectRequest,Request.map(_.RequestType_isWrite))
      WriteRequest.bits.RequestData := PriorityMux(SelectRequest,Request.map(_.RequestData))
      WriteRequest.valid := true.B
      //只有fire了才能继续
      when(WriteRequest.fire && io.LocalMMUIO.ConherentRequsetSourceID.valid){
        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Store<%d>]WriteRequest: RequestVirtualAddr= %x, RequestConherent= %x,RequestSourceID= %x,RequestType_isWrite= %x\n", io.DebugInfo.DebugTimeStampe, WriteRequest.bits.RequestVirtualAddr, WriteRequest.bits.RequestConherent, WriteRequest.bits.RequestSourceID, WriteRequest.bits.RequestType_isWrite)
        }

        FireTimes := FireTimes + 1.U
        when(FireTimes === (Per_SCP_Load_Write_Memory_Time-1).U){
          FireTimes := 0.U
          FromScratchpadReadFIFOTail := WrapInc(FromScratchpadReadFIFOTail, CMemoryLoaderReadFromScratchpadFIFODepth)
          TotalStoreSize := TotalStoreSize + 1.U
          //输出完成的写回次数
          if (YJPCMLDebugEnable)
          {
            printf("[CMemoryLoader_Store<%d>]TotalStoreSize: %x\n", io.DebugInfo.DebugTimeStampe, TotalStoreSize)
          }
          when(TotalStoreSize === Max_Load_SCP_Time - 1.U){
            memorystore_state := s_store_end

            if (YJPCMLDebugEnable)
            {
              printf("[CMemoryLoader_Store<%d>]StoreEnd\n",io.DebugInfo.DebugTimeStampe)
            }
          }
        }


        CurrentStore_BlockTensor_Reduce_DIM_Iter := CurrentStore_BlockTensor_Reduce_DIM_Iter + Per_Store_ReduceDim_Iter
        when(CurrentStore_BlockTensor_Reduce_DIM_Iter === Max_BlockTensor_Reduce_DIM - Per_Store_ReduceDim_Iter){
          CurrentStore_BlockTensor_Reduce_DIM_Iter := 0.U
          CurrentStore_BlockTensor_Major_DIM_Iter := CurrentStore_BlockTensor_Major_DIM_Iter + 1.U
        }

        if (YJPCMLDebugEnable)
        {
          printf("[CMemoryLoader_Store<%d>]CurrentStore_BlockTensor_Major_DIM_Iter: %x, CurrentStore_BlockTensor_Reduce_DIM_Iter: %x\n", io.DebugInfo.DebugTimeStampe, CurrentStore_BlockTensor_Major_DIM_Iter, CurrentStore_BlockTensor_Reduce_DIM_Iter)
        }
      }
    }
  }.elsewhen(memorystore_state === s_store_end){
    // memorystore_state := s_store_end
    io.ConfigInfo.MicroTaskEndValid := true.B
    when(io.ConfigInfo.MicroTaskEndReady && io.ConfigInfo.MicroTaskEndValid){
      memorystore_state := s_store_idle
      state := s_idle
      if (YJPCMLDebugEnable)
      {
        printf("[CMemoryLoader_Store<%d>]Store Finish\n",io.DebugInfo.DebugTimeStampe)
      }
    }
  }.otherwise{
    //闲闲没事做
  }







}