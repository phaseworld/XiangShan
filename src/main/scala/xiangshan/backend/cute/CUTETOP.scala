package xiangshan.backend.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._
// import scala.collection.parallel.Task

class CUTETopIO() extends Bundle{
  val mmu2llc = Flipped(new MMU2TLIO)
  val ctrl2top = Flipped(new YGJKControl)
}
class CUTEV2Top(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new CUTETopIO)

  //TODO:init DontCare很危险
  // io.cmd.acc_req_a.bits := DontCare
  // io.cmd.acc_req_a.valid := DontCare
  // io.cmd.acc_req_b.bits := DontCare
  // io.cmd.acc_req_b.valid := DontCare
  // io.cmd.req_id := DontCare
  // io.ctl.acc_running := DontCare



  val ASPad_0 = Module(new AScratchpad).io //double buffer
  val ASPad_1 = Module(new AScratchpad).io //double buffer
  val ADC = Module(new ADataController).io
  val AML = Module(new AMemoryLoader).io

  val BSPad_0 = Module(new BScratchpad).io
  val BSPad_1 = Module(new BScratchpad).io
  val BDC = Module(new BDataController).io
  val BML = Module(new BMemoryLoader).io

  val CSPad_0 = Module(new CScratchpad).io
  val CSPad_1 = Module(new CScratchpad).io
  val CDC = Module(new CDataController).io
  val CML = Module(new CMemoryLoader).io

  val TaskCtrl = Module(new TaskController).io

  val MTE = Module(new MatrixTE).io

  val MMU = Module(new LocalMMU).io

  MTE.VectorA <> ADC.VectorA
  MTE.VectorB <> BDC.VectorB
  MTE.MatirxC <> CDC.Matrix_C
  MTE.MatrixD <> CDC.ResultMatrix_D
  MTE.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  MTE.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid
  ADC.ComputeGo := MTE.ComputeGo
  BDC.ComputeGo := MTE.ComputeGo
  CDC.ComputeGo := MTE.ComputeGo




  //ADC能不能切换ScarchPad，要协商的东西还挺多的。
  ASPad_0.ScarchPadIO.FromDataController <> ADC.FromScarchPadIO //有问题的写法，改成全0的输入
  ASPad_1.ScarchPadIO.FromDataController <> ADC.FromScarchPadIO
  when(TaskCtrl.TaskCtrlInfo.ScaratchpadChosen.ADataControllerChosenIndex === 0.U)
  {
    ASPad_0.ScarchPadIO.FromDataController <> ADC.FromScarchPadIO
    ASPad_0.ScarchPadIO.DataControllerValid := true.B
    ASPad_1.ScarchPadIO.DataControllerValid := false.B
    ASPad_1.ScarchPadIO.FromDataController.BankAddr.valid := false.B
  }.otherwise{
    ASPad_0.ScarchPadIO.DataControllerValid := false.B
    ASPad_0.ScarchPadIO.FromDataController.BankAddr.valid := false.B
    ASPad_1.ScarchPadIO.FromDataController <> ADC.FromScarchPadIO
    ASPad_1.ScarchPadIO.DataControllerValid := true.B

  }
  // ADC.FromScarchPadIO <> ASPad_0.ScarchPadIO.FromDataController
  // ADC.FromScarchPadIO <> Mux(, ASPad_0.ScarchPadIO.FromDataController, ASPad_1.ScarchPadIO.FromDataController)
  ADC.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  ADC.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid //TaskCtrl.ConfigInfo.valid
  ADC.CaculateEnd.ready := true.B //实际上要看具体的另外一个ScarchPad的memoryloader的任务是否完成了。//TODO:
  ADC.TaskEnd.bits := true.B //TODO:
  ADC.TaskEnd.valid := true.B //TODO:

  // ASPad.ScarchPadIO.FromMemoryLoader <> AML.ToScarchPadIO
  // ASPad.ScarchPadIO.FromDataController <> ADC.FromScarchPadIO

  // AML.ToScarchPadIO <> Mux(TaskCtrl.ConfigInfo.bits.ScaratchpadChosen.AMemoryLoaderChosenIndex === 0.U, ASPad_0.ScarchPadIO.FromMemoryLoader, ASPad_1.ScarchPadIO.FromMemoryLoader)
  // val ASPadDefaultInvaild = Wire(new AScarchPadIO)

  ASPad_0.ScarchPadIO.FromMemoryLoader <> AML.ToScarchPadIO
  ASPad_1.ScarchPadIO.FromMemoryLoader <> AML.ToScarchPadIO
  when(TaskCtrl.TaskCtrlInfo.ScaratchpadChosen.AMemoryLoaderChosenIndex === 0.U)
  {
    //TODO:改ScarchPad内的处理逻辑。
    ASPad_0.ScarchPadIO.MemoryLoaderValid := true.B
    ASPad_0.ScarchPadIO.FromMemoryLoader <> AML.ToScarchPadIO
    ASPad_1.ScarchPadIO.MemoryLoaderValid := false.B
    ASPad_1.ScarchPadIO.FromMemoryLoader.BankAddr.valid := false.B
  }.otherwise{
    ASPad_0.ScarchPadIO.MemoryLoaderValid := false.B
    ASPad_0.ScarchPadIO.FromMemoryLoader.BankAddr.valid := false.B
    ASPad_1.ScarchPadIO.FromMemoryLoader <> AML.ToScarchPadIO
    ASPad_1.ScarchPadIO.MemoryLoaderValid := true.B
  }
  AML.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  AML.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid
  AML.TaskEnd.bits := true.B //TODO:
  AML.TaskEnd.valid := true.B //TODO:
  AML.MemoryLoadEnd.ready := true.B //TODO:

  // BDC.FromScarchPadIO <> Mux(TaskCtrl.ConfigInfo.bits.ScaratchpadChosen.BDataControllerChosenIndex === 0.U, BSPad_0.ScarchPadIO.FromDataController, BSPad_1.ScarchPadIO.FromDataController)
  BSPad_0.ScarchPadIO.FromDataController <> BDC.FromScarchPadIO
  BSPad_1.ScarchPadIO.FromDataController <> BDC.FromScarchPadIO
  when(TaskCtrl.TaskCtrlInfo.ScaratchpadChosen.BDataControllerChosenIndex === 0.U)
  {
    BSPad_0.ScarchPadIO.DataControllerValid := true.B
    BSPad_0.ScarchPadIO.FromDataController <> BDC.FromScarchPadIO
    BSPad_1.ScarchPadIO.DataControllerValid := false.B
    BSPad_1.ScarchPadIO.FromDataController.BankAddr.valid := false.B
  }.otherwise{
    BSPad_0.ScarchPadIO.DataControllerValid := false.B
    BSPad_0.ScarchPadIO.FromDataController.BankAddr.valid := false.B
    BSPad_1.ScarchPadIO.DataControllerValid := true.B
    BSPad_1.ScarchPadIO.FromDataController <> BDC.FromScarchPadIO
  }
  BDC.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  BDC.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid
  BDC.CaculateEnd.ready := true.B //实际上要看具体的另外一个ScarchPad的memoryloader的任务是否完成了。//TODO:
  BDC.TaskEnd.bits := true.B //TODO:
  BDC.TaskEnd.valid := true.B //TODO:

  // BSPad.ScarchPadIO.FromMemoryLoader <> BML.ToScarchPadIO
  // BSPad.ScarchPadIO.FromDataController <> BDC.FromScarchPadIO

  // BML.ToScarchPadIO <> Mux(TaskCtrl.ConfigInfo.bits.ScaratchpadChosen.BMemoryLoaderChosenIndex === 0.U, BSPad_0.ScarchPadIO.FromMemoryLoader, BSPad_1.ScarchPadIO.FromMemoryLoader)
  BSPad_0.ScarchPadIO.FromMemoryLoader <> BML.ToScarchPadIO
  BSPad_1.ScarchPadIO.FromMemoryLoader <> BML.ToScarchPadIO
  when(TaskCtrl.TaskCtrlInfo.ScaratchpadChosen.BMemoryLoaderChosenIndex === 0.U)
  {
    //TODO:改ScarchPad内的处理逻辑。
    BSPad_0.ScarchPadIO.MemoryLoaderValid := true.B
    BSPad_0.ScarchPadIO.FromMemoryLoader <> BML.ToScarchPadIO
    BSPad_1.ScarchPadIO.MemoryLoaderValid := false.B
    BSPad_1.ScarchPadIO.FromMemoryLoader.BankAddr.valid := false.B
  }.otherwise{
    BSPad_0.ScarchPadIO.MemoryLoaderValid := false.B
    BSPad_1.ScarchPadIO.FromMemoryLoader <> BML.ToScarchPadIO
    BSPad_0.ScarchPadIO.FromMemoryLoader.BankAddr.valid := false.B
    BSPad_1.ScarchPadIO.MemoryLoaderValid := true.B
  }
  BML.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  BML.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid
  BML.TaskEnd.bits := true.B //TODO:
  BML.TaskEnd.valid := true.B //TODO:
  BML.MemoryLoadEnd.ready := true.B //TODO:

  // CDC.FromScarchPadIO <> Mux(TaskCtrl.ConfigInfo.bits.ScaratchpadChosen.CDataControllerChosenIndex === 0.U, CSPad_0.ScarchPadIO.FromDataController, CSPad_1.ScarchPadIO.FromDataController)
  CSPad_0.ScarchPadIO.FromDataController <> CDC.FromScarchPadIO
  CSPad_1.ScarchPadIO.FromDataController <> CDC.FromScarchPadIO
  when(TaskCtrl.TaskCtrlInfo.ScaratchpadChosen.CDataControllerChosenIndex === 0.U)
  {
    CSPad_0.ScarchPadIO.DataControllerValid := true.B
    CSPad_0.ScarchPadIO.FromDataController <> CDC.FromScarchPadIO
    CSPad_1.ScarchPadIO.DataControllerValid := false.B
    CSPad_1.ScarchPadIO.FromDataController.ReadWriteRequest := 0.U
  }.otherwise{
    CSPad_0.ScarchPadIO.DataControllerValid := false.B
    CSPad_0.ScarchPadIO.FromDataController.ReadWriteRequest := 0.U
    CSPad_1.ScarchPadIO.DataControllerValid := true.B
    CSPad_1.ScarchPadIO.FromDataController <> CDC.FromScarchPadIO
  }
  CDC.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  CDC.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid
  CDC.CaculateEnd.ready := true.B //实际上要看具体的另外一个ScarchPad的memoryloader的任务是否完成了。//TODO:
  CDC.TaskEnd.bits := true.B //TODO:
  CDC.TaskEnd.valid := true.B //TODO:

  // CSPad.ScarchPadIO.FromMemoryLoader <> CML.ToScarchPadIO
  // CSPad.ScarchPadIO.FromDataController <> CDC.FromScarchPadIO

  // CML.ToScarchPadIO <> Mux(TaskCtrl.ConfigInfo.bits.ScaratchpadChosen.CMemoryLoaderChosenIndex === 0.U, CSPad_0.ScarchPadIO.FromMemoryLoader, CSPad_1.ScarchPadIO.FromMemoryLoader)
  CSPad_0.ScarchPadIO.FromMemoryLoader <> CML.ToScarchPadIO
  CSPad_1.ScarchPadIO.FromMemoryLoader <> CML.ToScarchPadIO
  when(TaskCtrl.TaskCtrlInfo.ScaratchpadChosen.CMemoryLoaderChosenIndex === 0.U)
  {
    //TODO:改ScarchPad内的处理逻辑。
    CSPad_0.ScarchPadIO.MemoryLoaderValid := true.B
    CML.ToScarchPadIO.ReadWriteResponse := CSPad_0.ScarchPadIO.FromMemoryLoader.ReadWriteResponse
    CSPad_0.ScarchPadIO.FromMemoryLoader <> CML.ToScarchPadIO
    CSPad_1.ScarchPadIO.MemoryLoaderValid := false.B
    CSPad_1.ScarchPadIO.FromMemoryLoader.ReadWriteRequest := 0.U
    CSPad_1.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.BankAddr.valid := false.B
    CSPad_1.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data.valid := false.B
  }.otherwise{
    CSPad_0.ScarchPadIO.MemoryLoaderValid := false.B
    CSPad_0.ScarchPadIO.FromMemoryLoader.ReadWriteRequest := 0.U
    CSPad_0.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.BankAddr.valid := false.B
    CSPad_0.ScarchPadIO.FromMemoryLoader.WriteRequestToScarchPad.Data.valid := false.B
    CSPad_1.ScarchPadIO.MemoryLoaderValid := true.B
    CML.ToScarchPadIO.ReadWriteResponse := CSPad_1.ScarchPadIO.FromMemoryLoader.ReadWriteResponse
    CSPad_1.ScarchPadIO.FromMemoryLoader <> CML.ToScarchPadIO
  }
  CML.ConfigInfo.bits := TaskCtrl.ConfigInfo.bits
  CML.ConfigInfo.valid := TaskCtrl.ConfigInfo.valid
  CML.TaskEnd.bits := true.B //TODO:
  CML.TaskEnd.valid := true.B //TODO:
  CML.MemoryLoadEnd.ready := true.B //TODO:

  TaskCtrl.ConfigInfo.ready := ADC.ConfigInfo.ready && BDC.ConfigInfo.ready && CDC.ConfigInfo.ready && MTE.ConfigInfo.ready && AML.ConfigInfo.ready && BML.ConfigInfo.ready && CML.ConfigInfo.ready

  io.mmu2llc <> MMU.LastLevelCacheTLIO
  AML.LocalMMUIO <> MMU.ALocalMMUIO
  BML.LocalMMUIO <> MMU.BLocalMMUIO
  CML.LocalMMUIO <> MMU.CLocalMMUIO
  MMU.Config := TaskCtrl.ConfigInfo.bits.MMUConfig
  io.ctrl2top <> TaskCtrl.ygjkctrl //指令宏码送入，taskctrl指令微码送出到各个模块，做到可配可能需要一个大模块做控制，其他小模块能从不同LLCnode发出访存请求的设计
  TaskCtrl.TaskCtrlInfo.ADC.TaskEnd <> ADC.TaskEnd
  TaskCtrl.TaskCtrlInfo.BDC.TaskEnd <> BDC.TaskEnd
  TaskCtrl.TaskCtrlInfo.CDC.TaskEnd <> CDC.TaskEnd
  TaskCtrl.TaskCtrlInfo.ADC.ComputeEnd <> ADC.CaculateEnd
  TaskCtrl.TaskCtrlInfo.BDC.ComputeEnd <> BDC.CaculateEnd
  TaskCtrl.TaskCtrlInfo.CDC.ComputeEnd <> CDC.CaculateEnd
  TaskCtrl.TaskCtrlInfo.AML.LoadEnd <> AML.MemoryLoadEnd
  TaskCtrl.TaskCtrlInfo.BML.LoadEnd <> BML.MemoryLoadEnd
  TaskCtrl.TaskCtrlInfo.CML.LoadEnd <> CML.MemoryLoadEnd
  TaskCtrl.TaskCtrlInfo.AML.TaskEnd <> AML.TaskEnd
  TaskCtrl.TaskCtrlInfo.BML.TaskEnd <> BML.TaskEnd
  TaskCtrl.TaskCtrlInfo.CML.TaskEnd <> CML.TaskEnd





}


