package xiangshan.backend.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._
//import boom.util._

//AMemoryLoader，用于加载A矩阵的数据，供给Scratchpad使用
//从不同的存储介质中加载数据，供给Scratchpad使用

//主要是从外部接口加载数据
//需要一个加速器整体的访存模块，接受MemoryLoader的请求，然后根据请求的地址，返回数据，MeomoryLoader发出虚拟地址
//这里其实涉及到一个比较隐蔽的问题，就是怎么设置这些页表来防止Linux的一些干扰，如SWAP、Lazy、CopyOnWrite等,这需要一系列的操作系统的支持
//本地的mmu会完成虚实地址转换，根据memoryloader的请求，选择从不同的存储介质中加载数据

//在本地最基础的是完成整体Tensor的加载，依据Scarchpad的设计，完成Tensor的切分以及将数据的填入Scaratchpad

//注意，数据的reorder是可以离线完成的！这也属于编译器的一环。

class CSourceIdSearch extends Bundle with HWParameters{
  val ScratchpadBankId =UInt(log2Ceil(CScratchpadNBanks).W)
  val ScratchpadAddr = UInt(log2Ceil(CScratchpadBankNEntrys).W)
  val FIFOIndex = UInt(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W)
}

class CMemoryLoader(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new Bundle{
    val ToScarchPadIO = Flipped(new CMemoryLoaderScaratchpadIO)
    val ConfigInfo = Flipped(DecoupledIO(new ConfigInfoIO))
    val LocalMMUIO = Flipped(new LocalMMUIO)
    val MemoryLoadEnd = DecoupledIO(Bool())
    val TaskEnd = Flipped(DecoupledIO(Bool()))
  })

  io.ConfigInfo.ready := false.B
  io.MemoryLoadEnd.valid := false.B
  io.MemoryLoadEnd.bits := false.B
  io.TaskEnd.ready := false.B
  io.ToScarchPadIO.ReadRequestToScarchPad.FullBankLoad := false.B
  io.ToScarchPadIO.ReadRequestToScarchPad.BankAddr.valid := false.B
  io.ToScarchPadIO.ReadRequestToScarchPad.BankAddr.bits := 0.U
  io.ToScarchPadIO.ReadRequestToScarchPad.BankId.valid := false.B
  io.ToScarchPadIO.ReadRequestToScarchPad.BankId.bits := 0.U
  io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr.valid := false.B
  io.ToScarchPadIO.WriteRequestToScarchPad.Data.valid := false.B
  io.ToScarchPadIO.WriteRequestToScarchPad.BankAddr.bits := 0.U
  io.ToScarchPadIO.WriteRequestToScarchPad.Data.bits := 0.U.asTypeOf(io.ToScarchPadIO.WriteRequestToScarchPad.Data.bits)
  io.ToScarchPadIO.Chosen := true.B
  io.LocalMMUIO.Request.bits.RequestConherent := false.B
  io.LocalMMUIO.Request.bits.RequestData := 0.U
  io.LocalMMUIO.Request.bits.RequestSourceID := 0.U
  io.LocalMMUIO.Request.bits.RequestType_isWrite := false.B
  io.LocalMMUIO.Request.bits.RequestVirtualAddr := false.B

  // io.ToScarchPadIO
  // io.LocalMMUIO := 0.U



  // val ScaratchpadBankAddr = io.ToScarchPadIO.BankAddr
  // val ScaratchpadData = io.ToScarchPadIO.Data
  // val ScaratchpadChosen = io.ToScarchPadIO.Chosen

  val ConfigInfo = io.ConfigInfo.bits

  val ApplicationTensor_M = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))
  val ApplicationTensor_N = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))
  val ApplicationTensor_K = RegInit(0.U(ApplicationMaxTensorSizeBitSize.W))

  val Tensor_C_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))
  val Tensor_D_BaseVaddr = RegInit(0.U(MMUAddrWidth.W))


  //任务状态机 先来个简单的，顺序读取所有分块矩阵
  val s_idle :: s_mm_task :: s_write :: Nil = Enum(3) //TODO:新增状态，这里要加各种计算状态，mm，sliding window之类的
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

  val HasScarhpadRead = WireInit(false.B)
  val HasScarhpadWrite = WireInit(false.B)
  io.ToScarchPadIO.ReadWriteRequest := Cat(HasScarhpadRead,Cat(HasScarhpadWrite,Cat(0.U(1.W),0.U(1.W))))

  //TODO:设定内存中的数据排布，好让数据进行读取，reorder。目前最基础的，memory里的就和Scarchpad里的一样
  //TODO:读取的话，就每次读取256bit对齐的数据，然后塞到指定bank的指定addr上。
  //TODO:Taskcontroller负责对数据切块，并提供MemoryLoader的配置信息，起始位置，大小，数据排布等等

  when(state === s_idle)
  {
    io.ConfigInfo.ready := true.B
  }
  //TrickTODO:添加configinstfifo，就可以完成大任务流水拉！！
  //如果configinfo有效

  when(io.ConfigInfo.fire){
    when(io.ConfigInfo.bits.taskType === CUTETaskType.TaskTypeMatrixMul){
      when(state === s_idle){
        state := s_mm_task
        when(io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType === CMemoryLoaderTaskType.TaskTypeTensorLoad){
          memoryload_state := s_load_init
          Tensor_Block_BaseAddr := io.ConfigInfo.bits.ApplicationTensor_C.BlockTensor_C_BaseVaddr
          IsConherent := io.ConfigInfo.bits.ApplicationTensor_C.Conherent
          MemoryOrder_LoadConfig := io.ConfigInfo.bits.ApplicationTensor_C.MemoryOrder
        }.elsewhen(io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType === CMemoryLoaderTaskType.TaskTypeTensorStore){
          memorystore_state := s_store_init
          Tensor_Block_BaseAddr := io.ConfigInfo.bits.ApplicationTensor_D.BlockTensor_D_BaseVaddr
          IsConherent := io.ConfigInfo.bits.ApplicationTensor_D.Conherent
          MemoryOrder_LoadConfig := io.ConfigInfo.bits.ApplicationTensor_D.MemoryOrder
        }.otherwise{
          //闲闲没事做
        }
        ApplicationTensor_M := io.ConfigInfo.bits.ApplicationTensor_M
        ApplicationTensor_N := io.ConfigInfo.bits.ApplicationTensor_N
        ApplicationTensor_K := io.ConfigInfo.bits.ApplicationTensor_K
        Tensor_C_BaseVaddr := io.ConfigInfo.bits.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr
        Tensor_D_BaseVaddr := io.ConfigInfo.bits.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr
      }
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
  val TotalLoadSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共要加载的数据量
  val TotalRequestSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共要加载的数据量
  val CurrentLoaded_BlockTensor_M_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val CurrentLoaded_BlockTensor_N_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

  //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
  //用sourceid做索引，存储Scarchpad的地址和bank号，是一组寄存器

  // val SoureceIdSearchTable = VecInit(Seq.fill(SoureceMaxNum){RegInit(new CSourceIdSearch)})
  val SoureceIdSearchTable = RegInit(VecInit(Seq.fill(SoureceMaxNum)(0.U((new CSourceIdSearch).getWidth.W))))


  //读数的FIFO
  //...这个fifo真的很亏啊...
  val FromMemoryLoaderReadFIFO = RegInit(VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(VecInit(Seq.fill(CScratchpadNBanks)(0.U(LLCDataWidth.W))))))
  //TODO:确认Valid是有效的，需要同时两个Valid都有效
  val FromMemoryLoaderReadFIFOValid = RegInit(VecInit(Seq.fill(CMemoryLoaderReadFromMemoryFIFODepth)(VecInit(Seq.fill(CScratchpadNBanks)(false.B)))))
  val FromMemoryLoaderReadFIFOHead = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))
  val FromMemoryLoaderReadFIFOTail = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))
  val FromMemoryLoaderReadFIFOFull = FromMemoryLoaderReadFIFOTail === WrapInc(FromMemoryLoaderReadFIFOHead, CMemoryLoaderReadFromMemoryFIFODepth)
  val SafeFromMemoryLoaderReadFIFOFull = FromMemoryLoaderReadFIFOTail === WrapInc(WrapInc(FromMemoryLoaderReadFIFOHead, CMemoryLoaderReadFromMemoryFIFODepth),CMemoryLoaderReadFromMemoryFIFODepth)
  val FromMemoryLoaderReadFIFOEmpty = FromMemoryLoaderReadFIFOHead === FromMemoryLoaderReadFIFOTail
  //用于确认FIFO内是否能接住更多的请求
  val InflightMemoryRequest = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromMemoryFIFODepth).W))
  //读数请求
  val ReadRequest = io.LocalMMUIO.Request
  ReadRequest.valid := false.B
  when(memoryload_state === s_load_init){
    memoryload_state := s_load_working
    TotalLoadSize := 0.U
    CurrentLoaded_BlockTensor_M_Iter := 0.U
    CurrentLoaded_BlockTensor_N_Iter := 0.U
  }.elsewhen(memoryload_state === s_load_working){
    //根据不同的MemoryOrder，执行不同的访存模式
    when(MemoryOrder_LoadConfig === MemoryOrderType.OrderType_Mb_Nb)
    {
      //输出head和tial
      // printf("[CMemoryLoader]FromMemoryLoaderReadFIFOHead: %x, FromMemoryLoaderReadFIFOTail: %x\n", FromMemoryLoaderReadFIFOHead, FromMemoryLoaderReadFIFOTail)
      // //输出FIFO的状态
      // printf("[CMemoryLoader]FromMemoryLoaderReadFIFOFull: %x, FromMemoryLoaderReadFIFOEmpty: %x\n", FromMemoryLoaderReadFIFOFull, FromMemoryLoaderReadFIFOEmpty)
      // //输出safeFULL
      // printf("[CMemoryLoader]SafeFromMemoryLoaderReadFIFOFull: %x\n", SafeFromMemoryLoaderReadFIFOFull)
      //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
      //TODO:注意所有的乘法电路的代价，和EDA存在的优化的可能性
      //担心乘法电路延迟，可以提前几个周期将乘法结果算好
      //TODO:注意这里的分块逻辑/地址拼接的逻辑，我们在设计MemoryOrderType分块的逻辑时，要考虑到这里的求地址的电路逻辑，是可以减少这部分的乘法电路的逻辑的
      //注意ScaratchPad内的存数的状态

      //数据在CScarachpad中的编排
      //数据会先排N，再排M
      //   N 0 1 2 3 4 5 6 7     CScaratchpadData里的排布
      // M                               {bank    [0]         [1]}
      // 0   0 1 2 3 4 5 6 7   |addr    0 |    0123,89ab   ghij,opgr
      // 1   8 9 a b c d e f   |        1 |    4567,cdef   klmn,stuv
      // 2   g h i j k l m n   |        2 |    wxyz,!...   @...,#...
      // 3   o p g r s t u v   |        3 |    ....,....   ....,....
      // 4   w x y z .......   |        4 |    ....,....   ....,....
      // 5   !..............   |        5 |    ....,....   ....,....
      // 6   @..............   |        6 |    ....,....   ....,....
      // 7   #..............   |        7 |    ....,....   ....,....
      // 8   $..............   | ....................................

      //其在内存中也是这样排布的 0123，89ab，ghij，opgr，4567，cdef，klmn，stuv，wxyz，!....~~~~
      //0123,89ab是一个LLC内对齐的内容，是LLC的一行，也是SRAM的BankLine的大小
      //所以这里的访存总次数是Tensor_M * Tensor_N / (BankEntrySizeByte / ResultWidthByte)
      //每次访存得到一个BankLine的数据，然后填入ScartchPad的一个BankLine中
      //Trick目前在这里没有reorder的需求，所以我们直接填入ScartchPad就可以了
      //如果有reorder的需求，我们需要在这里弄一个选择器，从不同的8个4byte里取数，重排位置，然后再填入ScartchPad
      val MaxRequestIter = Tensor_M * Tensor_N * ResultWidthByte / (CScratchpadEntryByteSize) //总共要发出的访存请求的次数
      val RequestScratchpadBankId = TotalRequestSize % CScratchpadNBanks.U //访存请求落在哪个ScratchpadBank上
      val RequestScratchpadAddr = TotalRequestSize / CScratchpadNBanks.U

      ReadRequest.bits.RequestVirtualAddr := Tensor_Block_BaseAddr + TotalRequestSize * CScratchpadEntryByteSize.U
      //TODO:nonConherent的设计，需要让edge支持burst的访存请求，这个估计需要从L2Cache里抄

      val CurrentBankID = RequestScratchpadBankId
      val CurrentFIFOIndex = FromMemoryLoaderReadFIFOHead

      val sourceId = Mux(IsConherent,io.LocalMMUIO.ConherentRequsetSourceID,io.LocalMMUIO.nonConherentRequsetSourceID)


      ReadRequest.bits.RequestConherent := IsConherent
      ReadRequest.bits.RequestSourceID := sourceId.bits
      ReadRequest.bits.RequestType_isWrite := false.B
      ReadRequest.valid := !SafeFromMemoryLoaderReadFIFOFull && (TotalRequestSize < MaxRequestIter.U)
      //输出valid和fifofull
      // printf("[CMemoryLoader]ReadRequestValid: %x, FromMemoryLoaderReadFIFOFull: %x\n", ReadRequest.valid, SafeFromMemoryLoaderReadFIFOFull)
      //输出ReadRequest、maxRequestIter、TotalRequestSize、RequestScratchpadBankId、CurrentBankID、CurrentFIFOIndex
      // printf("[CMemoryLoader]ReadRequest: %x, MaxRequestIter: %x, TotalRequestSize: %x, RequestScratchpadBankId: %x, CurrentBankID: %x, CurrentFIFOIndex: %x\n", ReadRequest.bits.RequestVirtualAddr, MaxRequestIter.U, TotalRequestSize, RequestScratchpadBankId, CurrentBankID, CurrentFIFOIndex)

      //确定这个访存请求一定会发出
      when(ReadRequest.ready){
        val TableItem = Wire(new CSourceIdSearch)
        TableItem.ScratchpadBankId := RequestScratchpadBankId
        TableItem.FIFOIndex := CurrentFIFOIndex
        TableItem.ScratchpadAddr := RequestScratchpadAddr
        SoureceIdSearchTable(sourceId.bits) := TableItem.asUInt
        // SoureceIdSearchTable(sourceId.bits).ScratchpadBankId := RequestScratchpadBankId
        // SoureceIdSearchTable(sourceId.bits).ScratchpadAddr := RequestScratchpadAddr
        // SoureceIdSearchTable(sourceId.bits).FIFOIndex := CurrentFIFOIndex
        //如果当前的BankID就是等于最后一个，那就可以使用一个新的FIFO Entry，如果FIFO满了就不能发出请求！！
        when(CurrentBankID === (CScratchpadNBanks-1).U){
          FromMemoryLoaderReadFIFOHead := WrapInc(FromMemoryLoaderReadFIFOHead, CMemoryLoaderReadFromMemoryFIFODepth)
        }
        //输出发出的访存请求
        // printf("[CMemoryLoader]RequestVirtualAddr: %x, RequestSourceID: %x, RequestConherent: %x, RequestType_isWrite: %x\n", ReadRequest.bits.RequestVirtualAddr, ReadRequest.bits.RequestSourceID, ReadRequest.bits.RequestConherent, ReadRequest.bits.RequestType_isWrite)
        //输出这次请求的TableItem
        // printf("[CMemoryLoader]TableItem: %x, %x, %x\n", TableItem.ScratchpadBankId, TableItem.ScratchpadAddr, TableItem.FIFOIndex)

        //只要这条取数指令可以被发出，就计算下一个访存请求的地址
        //TODO:这里数据读取量定死了，需要为了支持边界情况，改一改
        //不过我们保证了数据是256bit对齐的～剩下的就是Tensor_M和Tensor_K不满足的情况思考好就行了
        //输出request的次数
        // printf("[CMemoryLoader]TotalRequestSize: %x\n", TotalRequestSize)
        when(TotalRequestSize === (MaxRequestIter).U){
          //assert!
          //error!
        }.otherwise{
          TotalRequestSize := TotalRequestSize + 1.U
        }
      }
    }
    //接受访存的返回值
    //一个cam来存储访存请求的source_id对应的Scarchpad的地址和bank号
    //根据response的sourceid，找到对应的Scarchpad的地址和bank号，回填数据
    when(io.LocalMMUIO.Response.valid){
      val sourceId = io.LocalMMUIO.Response.bits.ReseponseSourceID
      val ScratchpadBankId = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).ScratchpadBankId
      val ScratchpadAddr = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).ScratchpadAddr
      val FIFOIndex = SoureceIdSearchTable(sourceId).asTypeOf(new CSourceIdSearch).FIFOIndex
      val ResponseData = io.LocalMMUIO.Response.bits.ReseponseData
      //输出localmmuio回的数据
      // printf("[CMemoryLoader]ResponseData: %x, ScratchpadBankId: %x, ScratchpadAddr: %x, FIFOIndex: %x\n", ResponseData, ScratchpadBankId, ScratchpadAddr, FIFOIndex)
      FromMemoryLoaderReadFIFO(FIFOIndex)(ScratchpadBankId) := ResponseData
      FromMemoryLoaderReadFIFOValid(FIFOIndex)(ScratchpadBankId) := true.B
    }

  }.elsewhen(memoryload_state === s_load_end){
    memoryload_state := s_load_end
    io.MemoryLoadEnd.valid := true.B
    when(io.MemoryLoadEnd.fire){
      memoryload_state := s_load_idle
      state := s_idle
    }
  }.otherwise{
    // memoryload_state := s_load_idle
    // io.ConfigInfo.ready := true.B
    // when(io.ConfigInfo.fire){
    //     when(io.ConfigInfo.bits.taskType === CUTETaskType.TaskTypeMatrixMul){
    //         when(state === s_idle){
    //             state := s_mm_task
    //             when(io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType === CMemoryLoaderTaskType.TaskTypeTensorLoad){
    //                 memoryload_state := s_load_init
    //             }.otherwise{
    //                 //闲闲没事做
    //             }
    //         }
    //     }
    // }
  }

  //FIFO回填数据到ScartchPad
  when(FromMemoryLoaderReadFIFOValid(FromMemoryLoaderReadFIFOTail).reduce(_&_)){

    val MaxLoadIter = (Tensor_M * Tensor_N * ResultWidthByte) / (CScratchpadEntryByteSize) / (CScratchpadNBanks)

    //修改io.ToScarchPadIO.ReadWriteRequest的特定位为1
    HasScarhpadWrite := true.B
    //输出io.ToScarchPadIO.ReadWriteResponse
    // printf("[CMemoryLoader]ReadWriteResponse: %x\n", io.ToScarchPadIO.ReadWriteResponse)
    when(io.ToScarchPadIO.ReadWriteResponse(ScaratchpadTaskType.WriteFromMemoryLoaderIndex) === true.B){
      //根据ScartchPad的仲裁结果，我们可以写入数据了
      val ScarchPadWriteRequest = io.ToScarchPadIO.WriteRequestToScarchPad
      val WriteData = FromMemoryLoaderReadFIFO(FromMemoryLoaderReadFIFOTail)
      ScarchPadWriteRequest.BankAddr.bits := TotalLoadSize
      ScarchPadWriteRequest.BankAddr.valid := true.B
      ScarchPadWriteRequest.Data.bits := WriteData
      ScarchPadWriteRequest.Data.valid := true.B
      FromMemoryLoaderReadFIFOValid(FromMemoryLoaderReadFIFOTail).foreach(_ := false.B)
      FromMemoryLoaderReadFIFOTail := WrapInc(FromMemoryLoaderReadFIFOTail, CMemoryLoaderReadFromMemoryFIFODepth)

      //输出当前的ScartchPad的地址和bank号，Load的总次数
      if (YJPDebugEnable)
      {
        printf("[CMemoryLoader]ScartchPadAddr: %x, TotalLoadSize: %d\n", ScarchPadWriteRequest.BankAddr.bits, TotalLoadSize)
      }

      //只要这条写入指令可以被发出，就计算下一个写入请求的地址
      when(TotalLoadSize === (MaxLoadIter).U){
        //assert!
        //error!
      }.otherwise{
        TotalLoadSize := TotalLoadSize + 1.U
        //状态机切换
        when(TotalLoadSize === (MaxLoadIter-1).U){
          memoryload_state := s_load_end
          if (YJPDebugEnable)
          {
            printf("[CMemoryLoader]LoadEnd\n")
          }
        }

      }
    }
  }

  //写数请求


  val TotalStoreSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共存储的数据量，这个参数表示已经对MMU发出的存储请求次数
  val TotalStoreRequestSize = RegInit(0.U((log2Ceil(Tensor_M*Tensor_N)).W)) //总共读取的请求数据量，这个参数表示已经对ScartchPad发出的读请求次数
  val CurrentStore_BlockTensor_M_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))
  val CurrentStore_BlockTensor_N_Iter = RegInit(0.U(ScaratchpadMaxTensorDimBitSize.W))

  //CMemoryLoaderReadFromScratchpadFIFODepth深度的fifo
  val FromScratchpadReadFIFO = RegInit(VecInit(Seq.fill(CMemoryLoaderReadFromScratchpadFIFODepth)(VecInit(Seq.fill(CScratchpadNBanks)(0.U(LLCDataWidth.W))))))
  val FromScratchpadReadFIFOValid = RegInit(VecInit(Seq.fill(CMemoryLoaderReadFromScratchpadFIFODepth)(false.B)))
  val FromScratchpadReadFIFOHead = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromScratchpadFIFODepth).W))
  val FromScratchpadReadFIFOTail = RegInit(0.U(log2Ceil(CMemoryLoaderReadFromScratchpadFIFODepth).W))
  val FromScratchpadReadFIFOFull = FromScratchpadReadFIFOTail === WrapInc(FromScratchpadReadFIFOHead, CMemoryLoaderReadFromScratchpadFIFODepth)
  val SafeFromScratchpadReadFIFOFull = FromScratchpadReadFIFOTail === WrapInc(WrapInc(FromScratchpadReadFIFOHead, CMemoryLoaderReadFromScratchpadFIFODepth),CMemoryLoaderReadFromScratchpadFIFODepth)
  val FromScratchpadReadFIFOEmpty = FromScratchpadReadFIFOHead === FromScratchpadReadFIFOTail

  val PreReadRequestValid = RegInit(false.B)
  val FireTimes = RegInit(0.U(log2Ceil(CScratchpadNBanks).W))

  when(memorystore_state === s_store_init){
    memorystore_state := s_store_working
    TotalStoreSize := 0.U
    CurrentStore_BlockTensor_M_Iter := 0.U
    CurrentStore_BlockTensor_N_Iter := 0.U
  }.elsewhen(memorystore_state === s_store_working){
    //根据不同的MemoryOrder，执行不同的访存模式
    when(MemoryOrder_LoadConfig === MemoryOrderType.OrderType_Mb_Nb)
    {
      //只要Request是ready，我们发出的访存请求就会被MMU送往总线，我们可以发出下一个访存请求
      val MaxStoreIter = Tensor_M * Tensor_N / (CScratchpadEntryByteSize / ResultWidthByte)
      val RequestScratchpadBankId = TotalStoreSize % CScratchpadNBanks.U
      val RequestScratchpadAddr = TotalStoreSize / CScratchpadNBanks.U
      val MaxSCAPRequestIter = Tensor_M * Tensor_N * ResultWidthByte / (CScratchpadEntryByteSize * CScratchpadNBanks) //总共要发对SCAP的访存次数

      //连续的向ScartchPad发出取数请求，送入fifo。
      //然后不断的将fifo中的数据送出到LLC

      //从ScartchPad中取数的逻辑

      //请求ScartchPad，需要从ScartchPad中取数，请求来自MemoryLoader
      //计算读取的ScartchPad的地址，计算读取的ScartchPad的bank号，这里可以把所有bank的数据取出来，也可以分不同bank取，建议所有bank都取出来，这样可以减少读取的次数
      val MaxLoadFromScratchpadIter = CScratchpadBankNEntrys


      //如果ScartchPad的仲裁结果允许我们读取数据
      PreReadRequestValid := false.B
      val FIFOIsSafe = !SafeFromScratchpadReadFIFOFull && !FromScratchpadReadFIFOFull
      when(FIFOIsSafe && TotalStoreRequestSize < MaxSCAPRequestIter.U)
      {
        HasScarhpadRead := true.B
      }
      when(io.ToScarchPadIO.ReadWriteResponse(ScaratchpadTaskType.ReadFromMemoryLoaderIndex) === true.B && FIFOIsSafe && TotalStoreRequestSize < MaxSCAPRequestIter.U){
        //根据ScartchPad的仲裁结果，我们可以读取数据了
        val ScarchPadReadRequest = io.ToScarchPadIO.ReadRequestToScarchPad
        ScarchPadReadRequest.BankAddr.bits := TotalStoreRequestSize
        ScarchPadReadRequest.BankAddr.valid := true.B
        ScarchPadReadRequest.BankId.bits := 0.U //全取
        ScarchPadReadRequest.BankId.valid := true.B
        ScarchPadReadRequest.FullBankLoad := true.B

        TotalStoreRequestSize := TotalStoreRequestSize + 1.U
        PreReadRequestValid := true.B
        if (YJPDebugEnable)
        {
          printf("[CMemoryLoader]ScartchPadReadRequest: %x, TotalStoreRequestSize: %d\n", ScarchPadReadRequest.BankAddr.bits, TotalStoreRequestSize)
        }
      }

      //只要ScaratchPad的数据读数有效，就可以将这个数置入fifo
      when(io.ToScarchPadIO.ReadRequestToScarchPad.ReadResponseData.valid){
        FromScratchpadReadFIFO(FromScratchpadReadFIFOHead) := io.ToScarchPadIO.ReadRequestToScarchPad.ReadResponseData.bits
        FromScratchpadReadFIFOValid(FromScratchpadReadFIFOHead) := true.B
        FromScratchpadReadFIFOHead := WrapInc(FromScratchpadReadFIFOHead, CMemoryLoaderReadFromScratchpadFIFODepth)
        //输出head
        if (YJPDebugEnable)
        {
          printf("[CMemoryLoader]FromScratchpadReadFIFOHead: %x\n", FromScratchpadReadFIFOHead)
        }
      }

      //只要fifo内的数据有效，就可以写入LLC
      val WriteRequest = io.LocalMMUIO.Request
      WriteRequest.valid := false.B
      when(!FromScratchpadReadFIFOEmpty){
        //这里需要进行CScratchpadBanks次数的写入，每次写入一个bank的数据
        //TODO:这里要是能是一个burst的写入就好了，这样可以减少总线的占用

        //得到一个writeRequest的list，然后对list进行map操作，得到一系列的writeRequest
        //然后对这些writeRequest进行一个循环，每次发出一个writeRequest

        val Request = List.fill(CScratchpadNBanks){Wire(new Bundle{
          val RequestVirtualAddr = UInt(MMUAddrWidth.W)
          val RequestConherent = Bool()
          val RequestData = UInt(MMUDataWidth.W)
          val RequestSourceID = UInt(SoureceMaxNumBitSize.W)
          val RequestType_isWrite = UInt(2.W) //0-读，1-写
        })}

        for(i <- 0 until CScratchpadNBanks){
          Request(i).RequestVirtualAddr := Tensor_Block_BaseAddr + (TotalStoreSize + i.U) * CScratchpadEntryByteSize.U
          Request(i).RequestConherent := IsConherent
          Request(i).RequestSourceID := 0.U
          Request(i).RequestType_isWrite := true.B
          Request(i).RequestData := FromScratchpadReadFIFO(FromScratchpadReadFIFOTail)(i)
        }



        val SelectRequest = UIntToOH(FireTimes,CScratchpadNBanks)

        WriteRequest.bits.RequestVirtualAddr := PriorityMux(SelectRequest,Request.map(_.RequestVirtualAddr))
        WriteRequest.bits.RequestConherent := PriorityMux(SelectRequest,Request.map(_.RequestConherent))
        WriteRequest.bits.RequestSourceID := PriorityMux(SelectRequest,Request.map(_.RequestSourceID))
        WriteRequest.bits.RequestType_isWrite := PriorityMux(SelectRequest,Request.map(_.RequestType_isWrite))
        WriteRequest.bits.RequestData := PriorityMux(SelectRequest,Request.map(_.RequestData))
        WriteRequest.valid := true.B
        //只有fire了才能继续
        when(WriteRequest.fire){
          FireTimes := FireTimes + 1.U
          when(FireTimes === (CScratchpadNBanks-1).U){
            FireTimes := 0.U
            FromScratchpadReadFIFOValid(FromScratchpadReadFIFOTail) := false.B
            FromScratchpadReadFIFOTail := WrapInc(FromScratchpadReadFIFOTail, CMemoryLoaderReadFromScratchpadFIFODepth)
            //输出tail
            if (YJPDebugEnable)
            {
              printf("[CMemoryLoader]FromScratchpadReadFIFOTail: %x\n", FromScratchpadReadFIFOTail)
            }
            TotalStoreSize := TotalStoreSize + CScratchpadNBanks.U
            when(TotalStoreSize === MaxStoreIter.U - CScratchpadNBanks.U){
              memorystore_state := s_store_end
            }
          }
          //输出完成的写回次数
          if (YJPDebugEnable)
          {
            printf("[CMemoryLoader]TotalStoreSize: %d  FireTimes := %d\n", TotalStoreSize, FireTimes)
          }
        }
      }
    }
  }.elsewhen(memorystore_state === s_store_end){
    memorystore_state := s_store_end
    io.MemoryLoadEnd.valid := true.B
    when(io.MemoryLoadEnd.fire){
      memoryload_state := s_store_idle
      state := s_idle
    }
  }.otherwise{
    // memorystore_state := s_store_idle
    // io.ConfigInfo.ready := true.B
    // when(io.ConfigInfo.fire){
    //     when(io.ConfigInfo.bits.taskType === CUTETaskType.TaskTypeMatrixMul){
    //         when(state === s_idle){
    //             state := s_mm_task
    //             when(io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType === CMemoryLoaderTaskType.TaskTypeTensorStore){
    //                 memorystore_state := s_store_init
    //             }.otherwise{
    //                 //闲闲没事做
    //             }
    //         }
    //     }
    // }
  }







}