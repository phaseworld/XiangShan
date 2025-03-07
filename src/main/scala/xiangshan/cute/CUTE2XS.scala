package xiangshan.cute

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.CuteCsrIO
import xiangshan.{HasXSParameter, XSBundle}
// import boom.acc._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._ //for rocc
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
//import boom.common._
import org.chipsalliance.cde.config._
//import boom.exu.ygjk.{YGJKParameters}


case object BuildMMacc extends Field[Parameters => Module]

//class WithCUTE extends Config((site,here,up) => {
//  case BuildRoCC => Seq(
//    (p:Parameters) => {
//      val XLEN = 64 // 寄存器位宽
//      val cute = LazyModule(new XS2CUTE(OpcodeSet.all)(p))
//      cute
//    }
//  )
//  case MMAccKey => true
//  case BuildDMAygjk => true
//})

//class XS2CUTE(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyModule
class XS2CUTE()(implicit p: Parameters) extends LazyModule
  with HWParameters{
  override lazy val module = new CUTETile(this)
  lazy val LLCMemPort = LazyModule(new Cute2TL)
//  tlNode := TLWidthWidget(LLCDataWidthByte) := LLCMemPort.node
}




class Cute2TL(implicit p: Parameters) extends LazyModule with HWParameters {
  lazy val module = new CUTE2TLImp(this)
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "Cute",
    sourceId = IdRange(0, LLCSourceMaxNum))))))
}

//这里的CUTE到LLC的节点
class CUTE2TLImp(outer: Cute2TL) extends LazyModuleImp(outer) with HWParameters{
  val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)

  val io = IO(new Bundle{
    val mmu = (new MMU2TLIO)
    val idle = Output(Bool())
  })

  val data = io.mmu.Request.bits.RequestData
  val busy = RegInit(VecInit(Seq.fill(LLCSourceMaxNum)(false.B)))
  val id = WireInit(0.U(LLCSourceMaxNumBitSize.W))

  val is_idle = !(busy.reduce(_|_))
  val is_full = busy.reduce(_&_)
  io.idle := is_idle


  for(i <- 0 until LLCSourceMaxNum){
    when(busy(i) === false.B){
      id := i.U
    }
  }
  io.mmu.ConherentRequsetSourceID.bits := id
  io.mmu.ConherentRequsetSourceID.valid := !is_full
  //输出是否sourceid已满的信息
  // printf("[CUTE2YGJK.node]is_full: %x\n", is_full)
  when(io.mmu.Request.fire){
    //输出mmu的sourceid的信息
    if (YJPDebugEnable)
    {
      printf("[CUTE2YGJK.node]sourceid: %x\n", io.mmu.ConherentRequsetSourceID.bits)
      //输出其他mmu的io.mmu.Request.bits的 所有信息,包含变量名
      printf("[CUTE2YGJK.node.io.mmu.Request.bits] RequestType_isWrite %x, RequestPhysicalAddr %x, RequestData %x\n", io.mmu.Request.bits.RequestType_isWrite, io.mmu.Request.bits.RequestPhysicalAddr, io.mmu.Request.bits.RequestData)
    }
  }



  when(!(is_full)){
    when(tl_out.a.fire){
      busy(id):=true.B
    }
  }.otherwise
  {
    //输出是否sourceid已满的信息
    if (YJPDebugEnable)
    {
      printf("[CUTE2YGJK.node]is_full: %x\n", is_full)
    }
  }
  //只要有请求，就输出一共有有多少个infligt的请求
  when(io.mmu.Request.valid || io.mmu.Response.valid){
    //统计busy，一共有多少个在飞行中的请求,及有多少个ture.B
    // printf("[CUTE2YGJK.node]busy: %d\n", busy.count(_ === true.B))
  }
  tl_out.d.ready := io.mmu.Response.ready
  when(tl_out.d.bits.opcode === TLMessages.AccessAck && tl_out.d.valid && busy(tl_out.d.bits.source) === true.B)//写回的ack，直接确认，不需要等待,怎么可能会不被响应？？
  {
    tl_out.d.ready := true.B
  }
  when(tl_out.d.fire){
    busy(tl_out.d.bits.source) := false.B
    if (YJPDebugEnable)
    {
      printf("[CUTE2YGJK.node]tl_out.d.fire: %x,tl_out.d.data: %x\n", tl_out.d.bits.source, tl_out.d.bits.data)
    }
  }

  if(YJPDebugEnable){
    val nack_cnt = RegInit(0.U(32.W))
    when(tl_out.d.valid && tl_out.d.ready === false.B){
      nack_cnt := nack_cnt + 1.U
      printf("[CUTE2YGJK.node]nack_cnt: %d\n", nack_cnt)
    }.otherwise(
      nack_cnt := 0.U
    )
  }

  tl_out.a.valid := io.mmu.Request.valid && !is_full
  tl_out.a.bits := Mux1H(Seq(
    (io.mmu.Request.bits.RequestType_isWrite === 0.U) -> edge.Get(id, io.mmu.Request.bits.RequestPhysicalAddr, log2Ceil(LLCDataWidthByte).U)._2,
    (io.mmu.Request.bits.RequestType_isWrite === 1.U) -> edge.Put(id, io.mmu.Request.bits.RequestPhysicalAddr, log2Ceil(LLCDataWidthByte).U, data)._2
  ))

  io.mmu.Response.valid := tl_out.d.valid && tl_out.d.bits.opcode === TLMessages.AccessAckData
  io.mmu.Request.ready := tl_out.a.ready && !(busy.reduce(_&_))
  io.mmu.Response.bits.ReseponseData := tl_out.d.bits.data
  io.mmu.Response.bits.ReseponseSourceID := tl_out.d.bits.source

  val time_stamp = RegInit(0.U(64.W))
  time_stamp := time_stamp + 1.U
  when(io.mmu.Response.fire){
    if (YJPDebugEnable)
    {
      printf("[CUTE2YGJK.node<%d>]io.mmu.Response.fire: %x, io.mmu.Response.bits.ReseponseData: %x\n", time_stamp, io.mmu.Response.bits.ReseponseSourceID, io.mmu.Response.bits.ReseponseData)
    }
  }

  when(io.mmu.Request.fire){
    if (YJPDebugEnable)
    {
      printf("[CUTE2YGJK.node<%d>]io.mmu.Request.fire: %x, io.mmu.Request.bits.RequestType_isWrite: %x, io.mmu.Request.bits.RequestPhysicalAddr: %x, io.mmu.Request.bits.RequestData: %x\n", time_stamp,io.mmu.ConherentRequsetSourceID.bits, io.mmu.Request.bits.RequestType_isWrite, io.mmu.Request.bits.RequestPhysicalAddr, io.mmu.Request.bits.RequestData)
    }
  }
}

class CUTEIO(implicit p: Parameters) extends XSBundle
{
  val csr = new CuteCsrIO
}

class CUTETile(outer: XS2CUTE)(implicit p: Parameters) extends LazyModuleImp(outer)
//  with YGJKParameters
  with HasXSParameter
  with HWParameters {
  val io = IO(new CUTEIO)

  val cmd = io.csr.tcmd
  val tbusy = cmd_to_tbusy(cmd)
  val tfunct = cmd_to_tfunct(cmd)
  val trd = cmd_to_trd(cmd)
  val topcode = cmd_to_topcode(cmd)

  val resp = io.csr.set_tresp
  val resp_valid = resp_to_valid(resp.bits)
  val resp_rd = resp_to_rd(resp.bits)

  val acc = Module(new CUTEV2Top)
  val mem = (outer.LLCMemPort.module)

  val rs1 = RegInit(0.U(XLEN.W))
  val rs2 = RegInit(0.U(XLEN.W))
  val rd_data = RegInit(0.U(XLEN.W))
  val rd = RegInit(0.U(5.W))
  val func = RegInit(0.U(7.W))
  val canResp = RegInit(false.B)
  val ac_busy = RegInit(false.B)
  val configV = RegInit(false.B)

  val count = RegInit(0.U(XLEN.W))
  when(ac_busy){
    count := count + 1.U
  }
  val compute = RegInit(0.U(XLEN.W))
  when(tbusy && ready && topcode === "h0B".U && tfunct === 0.U){
    compute := 0.U
  }.elsewhen(ac_busy){
    compute := compute + 1.U
  }

  val memNum_r = RegInit(0.U(XLEN.W))
  val memNum_w = RegInit(0.U(XLEN.W))

  val missAddr = RegInit(0.U(VAddrBits.W))

  val jk_idle :: jk_compute :: jk_resp :: jk_lmmu_miss :: Nil = Enum(4)
  val jk_state = RegInit(jk_idle)

  mem.io.mmu <> acc.io.mmu2llc


  //一拍的时间接受指令，下一拍的时间返回结果
  //后面可以设置成一个指令fifo
//  io.cmd.ready := !canResp

  val ready = !canResp
//  when(io.cmd.fire && io.cmd.bits.inst.xd === true.B){
  when(tbusy && ready){
    canResp := true.B
  }.elsewhen(canResp){
    canResp := false.B
  }
  // if (YJPDebugEnable)
  // {
  //     //输出io.cmd的信息和io.resp的信息
  //     printf("[CUTE2YGJK.top]io.cmd.fire: %x, io.cmd.bits.inst: %x, io.cmd.bits.rs1: %x, io.cmd.bits.rs2: %x, io.cmd.bits.inst.rd: %x, io.cmd.bits.inst.funct: %x\n", io.cmd.fire, io.cmd.bits.inst.asUInt, io.cmd.bits.rs1, io.cmd.bits.rs2, io.cmd.bits.inst.rd, io.cmd.bits.inst.funct)
  //     //输出valid和ready信息
  //     printf("[CUTE2YGJK.top]io.cmd.valid: %x, io.cmd.ready: %x, io.resp.valid: %x, io.resp.ready: %x\n", io.cmd.valid, io.cmd.ready, io.resp.valid, io.resp.ready)
  // }

  rd := trd    //下一拍一定会返回
  resp_valid := true.B
  resp_rd := rd
  io.csr.set_trespdata.bits := rd_data
  io.csr.set_trespdata.valid := canResp
  io.csr.set_tresp.valid := canResp

  when(tbusy && ready){
    io.csr.set_tcmdbusy.valid := true.B
    io.csr.set_tcmdbusy.bits := false.B
  }

  when(tbusy && ready && topcode === "h0B".U && tfunct === 1.U){ //查询加速器是否在运行
    rd_data := ac_busy
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 2.U){ //查询加速器运行时间
    rd_data := count
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 3.U){ //查询加速器对外访存读次数
    rd_data := memNum_r
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 4.U){ //查询加速器对外访存写次数
    rd_data := memNum_w
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 5.U){ //查询加速器计算时间
    rd_data := compute
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 6.U){ //查询CUTE宏指令的完成情况
    rd_data := acc.io.ctrl2top.InstFIFO_Finish
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 7.U){ //查询CUTE宏指令队列是否已满
    rd_data := acc.io.ctrl2top.InstFIFO_Full
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct === 8.U){ //查询CUTE宏指令队列目前有多少指令
    rd_data := acc.io.ctrl2top.InstFIFO_Info
  }.elsewhen(tbusy && ready && topcode === "h0B".U && tfunct >= 64.U){
    rd_data := acc.io.ctrl2top.cute_return_val
  }

  when(acc.io.mmu2llc.Request.fire){
    when(acc.io.mmu2llc.Request.bits.RequestType_isWrite === 0.U){
      memNum_r := memNum_r + 1.U
    }.elsewhen(acc.io.mmu2llc.Request.bits.RequestType_isWrite === 1.U){
      memNum_w := memNum_w + 1.U
    }
  }
//  io.interrupt := false.B
//  io.badvaddr_ygjk := Mux(jk_state=/=jk_resp, missAddr, missAddr+1.U)
  switch(jk_state){
    is(jk_idle){
      when(tbusy && ready && topcode === "h0B".U && tfunct(5,0) === 0.U){
        ac_busy := true.B
        jk_state := jk_compute
        count := 0.U
        memNum_r := 0.U
        memNum_w := 0.U
      }
    }

    is(jk_compute){
      if (YJPDebugEnable)
      {
        printf(p"[CUTE2YGJK.top]ac_busy = $ac_busy\n")
      }
      when(acc.io.ctrl2top.acc_running === false.B && mem.io.idle){
        jk_state := jk_resp
      }
    }

    is(jk_resp){
      //        io.interrupt := true.B
      ac_busy := false.B
      when(tbusy && ready && topcode === "h2B".U && tfunct === 0.U){
        // 收到中断响应
        jk_state := jk_idle
      }
    }
  }
  //opcode对应的是路由到某个加速器用的，CUSTOM0、CUSTOM1、CUSTOM2、CUSTOM3这四组opcode
  //我们这里默认使用opcode为0x0B的指令，将funct的最高位为1的指令作为配置指令。
  acc.io.ctrl2top.config.valid := tbusy && ready && topcode === "h0B".U && tfunct(6) === 1.U
  //输出指令信息,io.cmd.bits.inst.funct
  // printf("funct: %x\n", io.cmd.bits.inst.funct)
  when(tbusy && ready){
    if (YJPDebugEnable)
    {
      printf("CUTE: opcode: %x, rs1: %x, rs2: %x, rd: %x, funct: %x\n", topcode, io.csr.trs1, io.csr.trs2, trd, tfunct)
    }
  }
  acc.io.ctrl2top.config.bits.cfgData1 := io.csr.trs1
  acc.io.ctrl2top.config.bits.cfgData2 := io.csr.trs2
  acc.io.ctrl2top.config.bits.func := tfunct
  acc.io.ctrl2top.reset := false.B  //多次重启时置位，未实现

}
