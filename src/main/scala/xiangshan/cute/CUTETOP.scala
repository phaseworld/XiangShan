
package xiangshan.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._
// import scala.collection.parallel.Task

class YGJKControl extends Bundle{
  val reset = Output(Bool())
  val acc_running = Input(Bool())
  val InstFIFO_Info = Input((UInt(5.W)))
  val InstFIFO_Full = Input((Bool()))
  val InstFIFO_Finish = Input((UInt(5.W)))
  val cute_return_val = (Input(UInt(32.W)))
  val config  = Valid(new Bundle{
    val cfgData1 = UInt(64.W)
    val cfgData2 = UInt(64.W)
    val func = UInt(7.W)
  })
}

class CUTETopIO() extends Bundle{
  val mmu2llc = Flipped(new MMU2TLIO)
  val ctrl2top = Flipped(new YGJKControl)
}

class CUTEV2Top(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new CUTETopIO)

  val ASpad = Seq.tabulate(2)(i => Module(new AScratchpad(i))).toVector//双缓冲
  val ADC = Module(new ADataController)
  val AML = Module(new AMemoryLoader)

  val BSpad = Seq.tabulate(2)(i => Module(new BScratchpad)).toVector//双缓冲
  val BDC = Module(new BDataController)
  val BML = Module(new BMemoryLoader)

  val CSpad = Seq.tabulate(2)(i => Module(new CScratchpad)).toVector//双缓冲
  val CDC = Module(new CDataController)
  val CML = Module(new CMemoryLoader)

  val AOp = Module(new AfterOpsModule)
  val VecSIf = Module(new VectorStreamInterface)

  val TaskCtrl = Module(new TaskController)

  val MTE = Module(new MatrixTE)

  val MMU = Module(new LocalMMU)


  //debug reg
  val DebugTimeStampe = RegInit(0.U(32.W))
  DebugTimeStampe := DebugTimeStampe + 1.U

  TaskCtrl.io.DebugTimeStampe := DebugTimeStampe
  //ADC的默认输入
  ADC.io.FromScarchPadIO.Data.valid := false.B
  ADC.io.FromScarchPadIO.Data.bits := 0.U.asTypeOf(ADC.io.FromScarchPadIO.Data.bits)
  ADC.io.FromScarchPadIO.BankAddr.ready := false.B
  ADC.io.ConfigInfo <> TaskCtrl.io.ADC_MicroTask_Config
  ADC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

  //AML的默认输入
  AML.io.ConfigInfo <> TaskCtrl.io.AML_MicroTask_Config
  AML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
  AML.io.LocalMMUIO <> MMU.io.ALocalMMUIO

  //BDC的默认输入
  BDC.io.FromScarchPadIO.Data.valid := false.B
  BDC.io.FromScarchPadIO.Data.bits := 0.U.asTypeOf(BDC.io.FromScarchPadIO.Data.bits)
  BDC.io.FromScarchPadIO.BankAddr.ready := false.B
  BDC.io.ConfigInfo <> TaskCtrl.io.BDC_MicroTask_Config
  BDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe

  //BML的默认输入
  BML.io.ConfigInfo <> TaskCtrl.io.BML_MicroTask_Config
  BML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
  BML.io.LocalMMUIO <> MMU.io.BLocalMMUIO

  //CDC的默认输入
  CDC.io.FromScarchPadIO.ReadResponseData := 0.U.asTypeOf(CDC.io.FromScarchPadIO.ReadResponseData)
  CDC.io.FromScarchPadIO.ReadWriteResponse := 0.U.asTypeOf(CDC.io.FromScarchPadIO.ReadWriteResponse)
  CDC.io.ConfigInfo <> TaskCtrl.io.CDC_MicroTask_Config
  CDC.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
  CDC.io.AfterOpsInterface<>AOp.io.AfterOpsInterface

  //CML的默认输入
  CML.io.ConfigInfo <> TaskCtrl.io.CML_MicroTask_Config
  CML.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
  CML.io.LocalMMUIO <> MMU.io.CLocalMMUIO
  CML.io.ToScarchPadIO.ReadWriteResponse := 0.U
  CML.io.ToScarchPadIO.ReadRequestToScarchPad.ReadResponseData := 0.U.asTypeOf(CML.io.ToScarchPadIO.ReadRequestToScarchPad.ReadResponseData)

  //AOP的默认输入
  //AOp的要连接到vpu,目前先空接
  AOp.io.ConfigInfo <> TaskCtrl.io.AOP_MicroTask_Config
  AOp.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
  AOp.io.VectorInterface <> VecSIf.io.VectorInterface

  //VecSIF应该把VPU的输出输出接出去，但现在先空接
  val VPUIO = Module(new FakeVPU).io
  VPUIO.VPUInterface <> VecSIf.io.VPUInterface


  //MTE的默认输入
  MTE.io.VectorA <> ADC.io.VectorA
  MTE.io.VectorB <> BDC.io.VectorB
  MTE.io.MatirxC <> CDC.io.Matrix_C
  MTE.io.MatrixD <> CDC.io.ResultMatrix_D
  MTE.io.ConfigInfo <> TaskCtrl.io.MTE_MicroTask_Config
  MTE.io.DebugInfo.DebugTimeStampe := DebugTimeStampe
  ADC.io.ComputeGo := MTE.io.ComputeGo
  BDC.io.ComputeGo := MTE.io.ComputeGo
  CDC.io.ComputeGo := MTE.io.ComputeGo

  // TaskCtrl.io.MTE_MicroTask_Config.ready := true.B
  // ADC.io.VectorA.ready := true.B
  // BDC.io.VectorB.ready := true.B
  // CDC.io.Matrix_C.ready := true.B
  // CDC.io.ResultMatrix_D.bits := 0xdeadbeefL.U
  // CDC.io.ResultMatrix_D.valid := true.B
  // ADC.io.ComputeGo := true.B
  // BDC.io.ComputeGo := true.B
  // CDC.io.ComputeGo := true.B

  //后续需要连入CPU的MMU或者IOMMU
  MMU.io.Config.refillPaddr := 0.U
  MMU.io.Config.refillVaddr := 0.U
  MMU.io.Config.refill_v := false.B
  MMU.io.Config.useVM := false.B
  MMU.io.Config.useVM_v := false.B
  MMU.io.LastLevelCacheTLIO <> io.mmu2llc

  io.ctrl2top <> TaskCtrl.io.ygjkctrl

  //给每个SCP的输入进行defuat的赋值
  for (i <- 0 until 2){
    //ASpad
    //ADC的请求
    ASpad(i).io.ScarchPadIO.FromDataController.BankAddr.valid := false.B
    ASpad(i).io.ScarchPadIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(ASpad(i).io.ScarchPadIO.FromDataController.BankAddr.bits)
    // ASpad(i).io.ScarchPadIO.FromDataController.Data.valid := false.B
    // ASpad(i).io.ScarchPadIO.FromDataController.Data.bits := 0.U.asTypeOf(ASpad(i).io.ScarchPadIO.FromDataController.Data.bits)
    //AML的请求
    ASpad(i).io.ScarchPadIO.FromMemoryLoader.BankAddr.valid := false.B
    ASpad(i).io.ScarchPadIO.FromMemoryLoader.BankAddr.bits := 0.U.asTypeOf(ASpad(i).io.ScarchPadIO.FromMemoryLoader.BankAddr.bits)
    ASpad(i).io.ScarchPadIO.FromMemoryLoader.BankId.valid := false.B
    ASpad(i).io.ScarchPadIO.FromMemoryLoader.BankId.bits := 0.U.asTypeOf(ASpad(i).io.ScarchPadIO.FromMemoryLoader.BankId.bits)
    ASpad(i).io.ScarchPadIO.FromMemoryLoader.Data.valid := false.B
    ASpad(i).io.ScarchPadIO.FromMemoryLoader.Data.bits := 0.U.asTypeOf(ASpad(i).io.ScarchPadIO.FromMemoryLoader.Data.bits)

    //BSpad
    //BDC的请求
    BSpad(i).io.ScarchPadIO.FromDataController.BankAddr.valid := false.B
    BSpad(i).io.ScarchPadIO.FromDataController.BankAddr.bits := 0.U.asTypeOf(BSpad(i).io.ScarchPadIO.FromDataController.BankAddr.bits)
    // BSpad(i).io.ScarchPadIO.FromDataController.Data.valid := false.B
    // BSpad(i).io.ScarchPadIO.FromDataController.Data.bits := 0.U.asTypeOf(BSpad(i).io.ScarchPadIO.FromDataController.Data.bits)
    //BML的请求
    BSpad(i).io.ScarchPadIO.FromMemoryLoader.BankAddr.valid := false.B
    BSpad(i).io.ScarchPadIO.FromMemoryLoader.BankAddr.bits := 0.U.asTypeOf(BSpad(i).io.ScarchPadIO.FromMemoryLoader.BankAddr.bits)
    BSpad(i).io.ScarchPadIO.FromMemoryLoader.BankId.valid := false.B
    BSpad(i).io.ScarchPadIO.FromMemoryLoader.BankId.bits := 0.U.asTypeOf(BSpad(i).io.ScarchPadIO.FromMemoryLoader.BankId.bits)
    BSpad(i).io.ScarchPadIO.FromMemoryLoader.Data.valid := false.B
    BSpad(i).io.ScarchPadIO.FromMemoryLoader.Data.bits := 0.U.asTypeOf(BSpad(i).io.ScarchPadIO.FromMemoryLoader.Data.bits)

    //CSpad
    //CDC的请求
    CSpad(i).io.ScarchPadIO.FromDataController.ReadBankAddr := 0.U.asTypeOf(CSpad(i).io.ScarchPadIO.FromDataController.ReadBankAddr)
    CSpad(i).io.ScarchPadIO.FromDataController.WriteBankAddr := 0.U.asTypeOf(CSpad(i).io.ScarchPadIO.FromDataController.WriteBankAddr)
    CSpad(i).io.ScarchPadIO.FromDataController.WriteRequestData := 0.U.asTypeOf(CSpad(i).io.ScarchPadIO.FromDataController.WriteRequestData)
    //CML的请求
    CSpad(i).io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.BankAddr := 0.U.asTypeOf(CSpad(i).io.ScarchPadIO.FromMemoryLoader.ReadRequestToScarchPad.BankAddr)
    CSpad(i).io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.BankAddr := 0.U.asTypeOf(CSpad(i).io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.BankAddr)
    CSpad(i).io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data := 0.U.asTypeOf(CSpad(i).io.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data)
    //多个请求的仲裁器输入
    CSpad(i).io.ScarchPadIO.FromDataController.ReadWriteRequest := 0.U
    CSpad(i).io.ScarchPadIO.FromMemoryLoader.ReadWriteRequest := 0.U
  }

  //根据SCP_CtrlInfo的值，选择对应的SCP

  when (TaskCtrl.io.SCP_CtrlInfo.ADC_SCP_ID === 0.U){
    ADC.io.FromScarchPadIO <> ASpad(0).io.ScarchPadIO.FromDataController
  }.otherwise{
    ADC.io.FromScarchPadIO <> ASpad(1).io.ScarchPadIO.FromDataController
  }

  when (TaskCtrl.io.SCP_CtrlInfo.BDC_SCP_ID === 0.U){
    BDC.io.FromScarchPadIO <> BSpad(0).io.ScarchPadIO.FromDataController
  }.otherwise{
    BDC.io.FromScarchPadIO <> BSpad(1).io.ScarchPadIO.FromDataController
  }

  when (TaskCtrl.io.SCP_CtrlInfo.CDC_SCP_ID === 0.U){
    CDC.io.FromScarchPadIO <> CSpad(0).io.ScarchPadIO.FromDataController
  }.otherwise{
    CDC.io.FromScarchPadIO <> CSpad(1).io.ScarchPadIO.FromDataController
  }

  when (TaskCtrl.io.SCP_CtrlInfo.AML_SCP_ID === 0.U){
    AML.io.ToScarchPadIO <> ASpad(0).io.ScarchPadIO.FromMemoryLoader
  }.otherwise{
    AML.io.ToScarchPadIO <> ASpad(1).io.ScarchPadIO.FromMemoryLoader
  }

  when (TaskCtrl.io.SCP_CtrlInfo.BML_SCP_ID === 0.U){
    BML.io.ToScarchPadIO <> BSpad(0).io.ScarchPadIO.FromMemoryLoader
  }.otherwise{
    BML.io.ToScarchPadIO <> BSpad(1).io.ScarchPadIO.FromMemoryLoader
  }

  when (TaskCtrl.io.SCP_CtrlInfo.CML_SCP_ID === 0.U){
    CML.io.ToScarchPadIO <> CSpad(0).io.ScarchPadIO.FromMemoryLoader
  }.otherwise{
    CML.io.ToScarchPadIO <> CSpad(1).io.ScarchPadIO.FromMemoryLoader
  }




}


