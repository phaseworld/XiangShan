package xiangshan.backend.cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk._
//import firrtl.passes.memlib.Config

//TaskController代表,
class TaskController(implicit p: Parameters) extends Module with HWParameters{
  val io = IO(new Bundle{
    val ygjkctrl = Flipped(new YGJKControl)
    val ConfigInfo = DecoupledIO(new ConfigInfoIO)
    val TaskCtrlInfo = (new TaskCtrlInfo)
  })

  io.TaskCtrlInfo.AML.LoadEnd.ready := false.B
  io.TaskCtrlInfo.AML.TaskEnd.bits := false.B
  io.TaskCtrlInfo.AML.TaskEnd.valid := false.B
  io.TaskCtrlInfo.BML.LoadEnd.ready := false.B
  io.TaskCtrlInfo.BML.TaskEnd.bits := false.B
  io.TaskCtrlInfo.BML.TaskEnd.valid := false.B
  io.TaskCtrlInfo.CML.LoadEnd.ready := false.B
  io.TaskCtrlInfo.CML.TaskEnd.bits := false.B
  io.TaskCtrlInfo.CML.TaskEnd.valid := false.B

  io.TaskCtrlInfo.ADC.TaskEnd.valid := false.B
  io.TaskCtrlInfo.ADC.TaskEnd.bits := false.B
  io.TaskCtrlInfo.ADC.ComputeEnd.ready := false.B
  io.TaskCtrlInfo.BDC.TaskEnd.valid := false.B
  io.TaskCtrlInfo.BDC.TaskEnd.bits := false.B
  io.TaskCtrlInfo.BDC.ComputeEnd.ready := false.B
  io.TaskCtrlInfo.CDC.TaskEnd.valid := false.B
  io.TaskCtrlInfo.CDC.TaskEnd.bits := false.B
  io.TaskCtrlInfo.CDC.ComputeEnd.ready := false.B
  //就测试一个矩阵乘
  io.ConfigInfo.bits.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_A.BlockTensor_A_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_A.MemoryOrder := MemoryOrderType.OrderType_Mb_Kb
  io.ConfigInfo.bits.ApplicationTensor_A.Conherent := true.B

  io.ConfigInfo.bits.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_B.BlockTensor_B_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_B.MemoryOrder := MemoryOrderType.OrderType_Nb_Kb
  io.ConfigInfo.bits.ApplicationTensor_B.Conherent := true.B

  io.ConfigInfo.bits.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_C.BlockTensor_C_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_C.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
  io.ConfigInfo.bits.ApplicationTensor_C.Conherent := true.B

  io.ConfigInfo.bits.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_D.BlockTensor_D_BaseVaddr := (0x80000000L).U
  io.ConfigInfo.bits.ApplicationTensor_D.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
  io.ConfigInfo.bits.ApplicationTensor_D.Conherent := true.B

  io.ConfigInfo.bits.ApplicationTensor_M := Tensor_M.U
  io.ConfigInfo.bits.ApplicationTensor_N := Tensor_N.U
  io.ConfigInfo.bits.ApplicationTensor_K := Tensor_K.U

  io.ConfigInfo.bits.ScaratchpadTensor_K := Tensor_K.U
  io.ConfigInfo.bits.ScaratchpadTensor_N := Tensor_N.U
  io.ConfigInfo.bits.ScaratchpadTensor_M := Tensor_M.U

  io.ConfigInfo.bits.taskType := CUTETaskType.TaskTypeMatrixMul
  io.ConfigInfo.bits.dataType := ElementDataType.DataTypeUInt8




  //100000000个周期以后调换一次
  val test_count = RegInit(0.U(32.W))
  test_count := test_count + 1.U
  // val ctc = RegInit(0.U(1.W))
  // when(test_count === 100000000.U){
  //     ctc := ~ctc
  // }
  // when(test_count === 100000000.U){
  //     test_count := 0.U
  // }
  io.TaskCtrlInfo.ScaratchpadChosen.ADataControllerChosenIndex := 1.U
  io.TaskCtrlInfo.ScaratchpadChosen.BDataControllerChosenIndex := 1.U
  io.TaskCtrlInfo.ScaratchpadChosen.CDataControllerChosenIndex := 1.U

  io.TaskCtrlInfo.ScaratchpadChosen.AMemoryLoaderChosenIndex := 0.U
  io.TaskCtrlInfo.ScaratchpadChosen.BMemoryLoaderChosenIndex := 0.U
  io.TaskCtrlInfo.ScaratchpadChosen.CMemoryLoaderChosenIndex := 0.U

  io.ConfigInfo.bits.MMUConfig.refillVaddr := io.ygjkctrl.config.bits.cfgData1
  io.ConfigInfo.bits.MMUConfig.refillPaddr := io.ygjkctrl.config.bits.cfgData2
  io.ConfigInfo.bits.MMUConfig.refill_v := false.B
  io.ConfigInfo.bits.MMUConfig.useVM := false.B
  io.ConfigInfo.bits.MMUConfig.useVM_v := false.B

  io.ConfigInfo.bits.CMemoryLoaderConfig.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
  io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType := CMemoryLoaderTaskType.TaskTypeTensorLoad

  io.ConfigInfo.valid := false.B
  io.ConfigInfo.bits.ComputeGo := false.B

  val acc_run = RegInit(false.B)
  when(io.ygjkctrl.config.valid){
    acc_run := true.B
  }
  io.ygjkctrl.acc_running := acc_run

  // val configvalid = RegInit(false.B)
  val ApplicationTensor_A_BaseVaddr = RegInit(0.U(64.W))
  val ApplicationTensor_B_BaseVaddr = RegInit(0.U(64.W))
  val ApplicationTensor_C_BaseVaddr = RegInit(0.U(64.W))
  val ApplicationTensor_D_BaseVaddr = RegInit(0.U(64.W))
  val ApplicationTensor_M = RegInit(0.U(32.W))
  val ApplicationTensor_N = RegInit(0.U(32.W))
  val ApplicationTensor_K = RegInit(0.U(32.W))

  //任务状态机
  val task_idle :: task_compute :: task_resp :: Nil = Enum(3)
  val mm_task_init :: mm_task_compute :: mm_task_resp :: Nil = Enum(3)

  val acc_count = RegInit(0.U(32.W))
  val start = RegInit(false.B)

  when(start)
  {
    acc_count := acc_count + 1.U
  }

  val idel_test :: load_test :: compute_test :: store_test :: end_acc :: Nil = Enum(5)
  val test_state = RegInit(idel_test)

  when(io.ygjkctrl.config.valid)
  {
    //输出指令
    if (YJPDebugEnable)
    {
      printf("TaskController: func = %d, cfgData1 = %d, cfgData2 = %d\n", io.ygjkctrl.config.bits.func, io.ygjkctrl.config.bits.cfgData1, io.ygjkctrl.config.bits.cfgData2)
    }
    //opcode === 0,查询加速器状态，目前都在CUTE2YGJK.scala中实现
    //opcode === 2,硬件中断栈的中断响应
    //加速器内目前就先按func来区分


    //funct为func去除最高位的部分
    val funct = io.ygjkctrl.config.bits.func(5,0)
    //funct === 0,启动加速器
    //funct === 1，配置加速器，ATensor的配置信息,起始地址，存储一致性，数据排布，张量维度
    //funct === 2，配置加速器，BTensor的配置信息,起始地址，存储一致性，数据排布，张量维度
    //funct === 3，配置加速器，CTensor的配置信息,起始地址，存储一致性，数据排布，张量维度
    when(funct === 0.U)
    {
      //这里最好是生成一条VLSW送到加速器的指令buff里，然后在TaskController继续分解成不同期间的指令
      //目前先实现成单条指令触发
      io.ConfigInfo.valid := true.B
      io.ConfigInfo.bits.taskType := CUTETaskType.TaskTypeMatrixMul
      io.ConfigInfo.bits.dataType := ElementDataType.DataTypeUInt8
      io.ConfigInfo.bits.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := ApplicationTensor_A_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_A.BlockTensor_A_BaseVaddr := ApplicationTensor_A_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_K
      io.ConfigInfo.bits.ApplicationTensor_A.MemoryOrder := MemoryOrderType.OrderType_Mb_Kb
      io.ConfigInfo.bits.ApplicationTensor_A.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := ApplicationTensor_B_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_B.BlockTensor_B_BaseVaddr := ApplicationTensor_B_BaseVaddr + 0.U*ApplicationTensor_K*ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_B.MemoryOrder := MemoryOrderType.OrderType_Nb_Kb
      io.ConfigInfo.bits.ApplicationTensor_B.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := ApplicationTensor_C_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_C.BlockTensor_C_BaseVaddr := ApplicationTensor_C_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_C.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
      io.ConfigInfo.bits.ApplicationTensor_C.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := ApplicationTensor_D_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_D.BlockTensor_D_BaseVaddr := ApplicationTensor_D_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_D.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
      io.ConfigInfo.bits.ApplicationTensor_D.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_M := ApplicationTensor_M
      io.ConfigInfo.bits.ApplicationTensor_N := ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_K := ApplicationTensor_K
      io.ConfigInfo.bits.ScaratchpadTensor_K := Tensor_K.U
      io.ConfigInfo.bits.ScaratchpadTensor_N := Tensor_N.U
      io.ConfigInfo.bits.ScaratchpadTensor_M := Tensor_M.U

      test_state := load_test
      io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType := CMemoryLoaderTaskType.TaskTypeTensorLoad

      start := true.B

    }.elsewhen(funct === 1.U)
    {
      ApplicationTensor_A_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
      ApplicationTensor_M := io.ygjkctrl.config.bits.cfgData2(31,0)
      ApplicationTensor_N := io.ygjkctrl.config.bits.cfgData2(63,32)

    }.elsewhen(funct === 2.U)
    {
      ApplicationTensor_B_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
      ApplicationTensor_K := io.ygjkctrl.config.bits.cfgData2(31,0)
      // ApplicationTensor_N := io.ygjkctrl.config.bits.cfgData2(63,32)
    }.elsewhen(funct === 3.U)
    {
      ApplicationTensor_C_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
      ApplicationTensor_D_BaseVaddr := io.ygjkctrl.config.bits.cfgData2
      // ApplicationTensor_M := io.ygjkctrl.config.bits.cfgData2(31,0)
      // ApplicationTensor_N := io.ygjkctrl.config.bits.cfgData2(63,32)
    }.elsewhen(funct === 4.U)
    {
      ApplicationTensor_C_BaseVaddr := io.ygjkctrl.config.bits.cfgData1
      // ApplicationTensor_M := io.ygjkctrl.config.bits.cfgData2(31,0)
      // ApplicationTensor_N := io.ygjkctrl.config.bits.cfgData2(63,32)
    }.elsewhen(funct === 27.U)
    {
      io.ConfigInfo.bits.MMUConfig.refill_v := io.ygjkctrl.config.valid
      io.ConfigInfo.bits.MMUConfig.useVM := true.B
      io.ConfigInfo.bits.MMUConfig.useVM_v := true.B
    }
  }




  when(start === true.B && acc_count < 50000.U)
  {
    when(test_state === load_test && io.TaskCtrlInfo.AML.LoadEnd.valid === true.B && io.TaskCtrlInfo.BML.LoadEnd.valid === true.B && io.TaskCtrlInfo.CML.LoadEnd.valid === true.B)
    {
      io.TaskCtrlInfo.AML.LoadEnd.ready := true.B
      io.TaskCtrlInfo.BML.LoadEnd.ready := true.B
      io.TaskCtrlInfo.CML.LoadEnd.ready := true.B

      test_state := compute_test

      io.ConfigInfo.bits.ComputeGo := true.B
      io.ConfigInfo.valid := true.B

      io.ConfigInfo.bits.taskType := CUTETaskType.TaskTypeMatrixMul
      io.ConfigInfo.bits.dataType := ElementDataType.DataTypeUInt8
      io.ConfigInfo.bits.ApplicationTensor_A.ApplicationTensor_A_BaseVaddr := ApplicationTensor_A_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_A.BlockTensor_A_BaseVaddr := ApplicationTensor_A_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_K
      io.ConfigInfo.bits.ApplicationTensor_A.MemoryOrder := MemoryOrderType.OrderType_Mb_Kb
      io.ConfigInfo.bits.ApplicationTensor_A.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_B.ApplicationTensor_B_BaseVaddr := ApplicationTensor_B_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_B.BlockTensor_B_BaseVaddr := ApplicationTensor_B_BaseVaddr + 0.U*ApplicationTensor_K*ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_B.MemoryOrder := MemoryOrderType.OrderType_Nb_Kb
      io.ConfigInfo.bits.ApplicationTensor_B.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_C.ApplicationTensor_C_BaseVaddr := ApplicationTensor_C_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_C.BlockTensor_C_BaseVaddr := ApplicationTensor_C_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_C.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
      io.ConfigInfo.bits.ApplicationTensor_C.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := ApplicationTensor_D_BaseVaddr
      io.ConfigInfo.bits.ApplicationTensor_D.BlockTensor_D_BaseVaddr := ApplicationTensor_D_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_D.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
      io.ConfigInfo.bits.ApplicationTensor_D.Conherent := true.B
      io.ConfigInfo.bits.ApplicationTensor_M := ApplicationTensor_M
      io.ConfigInfo.bits.ApplicationTensor_N := ApplicationTensor_N
      io.ConfigInfo.bits.ApplicationTensor_K := ApplicationTensor_K
      io.ConfigInfo.bits.ScaratchpadTensor_K := Tensor_K.U
      io.ConfigInfo.bits.ScaratchpadTensor_N := Tensor_N.U
      io.ConfigInfo.bits.ScaratchpadTensor_M := Tensor_M.U

    }.elsewhen(test_state === compute_test)
    {
      io.TaskCtrlInfo.ScaratchpadChosen.ADataControllerChosenIndex := 0.U
      io.TaskCtrlInfo.ScaratchpadChosen.BDataControllerChosenIndex := 0.U
      io.TaskCtrlInfo.ScaratchpadChosen.CDataControllerChosenIndex := 0.U

      io.TaskCtrlInfo.ScaratchpadChosen.AMemoryLoaderChosenIndex := 1.U
      io.TaskCtrlInfo.ScaratchpadChosen.BMemoryLoaderChosenIndex := 1.U
      io.TaskCtrlInfo.ScaratchpadChosen.CMemoryLoaderChosenIndex := 1.U
      //用A0B0C0算，存C0
      when(io.TaskCtrlInfo.CDC.ComputeEnd.valid === true.B && io.TaskCtrlInfo.BDC.ComputeEnd.valid === true.B && io.TaskCtrlInfo.ADC.ComputeEnd.valid === true.B)
      {

        io.TaskCtrlInfo.ADC.ComputeEnd.ready := true.B
        io.TaskCtrlInfo.BDC.ComputeEnd.ready := true.B
        io.TaskCtrlInfo.CDC.ComputeEnd.ready := true.B
        io.TaskCtrlInfo.ADC.TaskEnd.valid := true.B
        io.TaskCtrlInfo.BDC.TaskEnd.valid := true.B
        io.TaskCtrlInfo.CDC.TaskEnd.valid := true.B
        io.TaskCtrlInfo.ADC.TaskEnd.bits := true.B
        io.TaskCtrlInfo.BDC.TaskEnd.bits := true.B
        io.TaskCtrlInfo.CDC.TaskEnd.bits := true.B
        io.TaskCtrlInfo.ScaratchpadChosen.ADataControllerChosenIndex := 1.U
        io.TaskCtrlInfo.ScaratchpadChosen.BDataControllerChosenIndex := 1.U
        io.TaskCtrlInfo.ScaratchpadChosen.CDataControllerChosenIndex := 1.U

        io.TaskCtrlInfo.ScaratchpadChosen.AMemoryLoaderChosenIndex := 0.U
        io.TaskCtrlInfo.ScaratchpadChosen.BMemoryLoaderChosenIndex := 0.U
        io.TaskCtrlInfo.ScaratchpadChosen.CMemoryLoaderChosenIndex := 0.U
        test_state := store_test
        io.ConfigInfo.bits.CMemoryLoaderConfig.TaskType := CMemoryLoaderTaskType.TaskTypeTensorStore
        io.ConfigInfo.bits.ApplicationTensor_D.ApplicationTensor_D_BaseVaddr := ApplicationTensor_D_BaseVaddr
        io.ConfigInfo.bits.ApplicationTensor_D.BlockTensor_D_BaseVaddr := ApplicationTensor_D_BaseVaddr + 0.U*ApplicationTensor_M*ApplicationTensor_N
        io.ConfigInfo.bits.ApplicationTensor_D.MemoryOrder := MemoryOrderType.OrderType_Mb_Nb
        io.ConfigInfo.bits.ApplicationTensor_D.Conherent := true.B
        io.ConfigInfo.valid := true.B
      }




    }.elsewhen(test_state === store_test)
    {
      //算完之后，将0存到内存
      io.TaskCtrlInfo.ScaratchpadChosen.ADataControllerChosenIndex := 1.U
      io.TaskCtrlInfo.ScaratchpadChosen.BDataControllerChosenIndex := 1.U
      io.TaskCtrlInfo.ScaratchpadChosen.CDataControllerChosenIndex := 1.U

      io.TaskCtrlInfo.ScaratchpadChosen.AMemoryLoaderChosenIndex := 0.U
      io.TaskCtrlInfo.ScaratchpadChosen.BMemoryLoaderChosenIndex := 0.U
      io.TaskCtrlInfo.ScaratchpadChosen.CMemoryLoaderChosenIndex := 0.U
      when(io.TaskCtrlInfo.CML.LoadEnd.valid === true.B)
      {
        test_state  := idel_test
        acc_run := false.B
      }
    }.otherwise{

    }
  }
  when(acc_count >= 50000.U)
  {
    start := false.B
    acc_run := false.B
  }
  // io.ConfigInfo.valid := configvalid
  // when(io.ConfigInfo.fire)
  // {
  //     configvalid := false.B
  // }

}
