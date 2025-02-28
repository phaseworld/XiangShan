package xiangshan.backend.cute

import chisel3._
import chisel3.util._
// import boom.acc._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._ //for rocc
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
//import xiangshan._
//import boom.common._
import org.chipsalliance.cde.config._
//import xiangshan.backend.cute._

case object BuildMMacc extends Field[Parameters => Module]

class WithCUTE extends Config((site,here,up) => {
  case BuildRoCC => Seq(
    (p:Parameters) => {
      val regWidth = 64 // 寄存器位宽
      val cute = LazyModule(new RoCC2CUTE(OpcodeSet.all)(p))
      cute
    }
  )
  case MMAccKey => true
  case BuildDMAygjk => true
})

class RoCC2CUTE(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes)
  with HWParameters{
  override lazy val module = new CUTETile(this)
  lazy val LLCMemPort = LazyModule(new Cute2TL)
  tlNode := TLWidthWidget(LLCDataWidthByte) := LLCMemPort.node
}




class Cute2TL(implicit p: Parameters) extends LazyModule with HWParameters {
  lazy val module = new CUTE2TLImp(this)
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
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

  when(tl_out.d.fire){
    //当拍必被接受
    busy(tl_out.d.bits.source) := false.B
  }

  tl_out.a.valid := io.mmu.Request.valid && !is_full
  tl_out.a.bits := Mux1H(Seq(
    (io.mmu.Request.bits.RequestType_isWrite === 0.U) -> edge.Get(id, io.mmu.Request.bits.RequestPhysicalAddr, log2Ceil(LLCDataWidthByte).U)._2,
    (io.mmu.Request.bits.RequestType_isWrite === 1.U) -> edge.Put(id, io.mmu.Request.bits.RequestPhysicalAddr, log2Ceil(LLCDataWidthByte).U, data)._2
  ))


  tl_out.d.ready := true.B
  io.mmu.Response.valid := tl_out.d.valid && tl_out.d.bits.opcode === TLMessages.AccessAckData
  io.mmu.Request.ready := tl_out.a.ready && !(busy.reduce(_&_))
  io.mmu.Response.bits.ReseponseData := tl_out.d.bits.data
  io.mmu.Response.bits.ReseponseSourceID := tl_out.d.bits.source
}



class CUTETile(outer: RoCC2CUTE)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with YGJKParameters
  with HasBoomCoreParameters
  with HWParameters {
  val acc = Module(new CUTEV2Top)
  val mem = (outer.LLCMemPort.module)

  val rs1 = RegInit(0.U(regWidth.W))
  val rs2 = RegInit(0.U(regWidth.W))
  val rd_data = RegInit(0.U(regWidth.W))
  val rd = RegInit(0.U(5.W))
  val func = RegInit(0.U(7.W))
  val canResp = RegInit(false.B)
  val ac_busy = RegInit(false.B)
  val configV = RegInit(false.B)

  val count = RegInit(0.U(regWidth.W))
  when(ac_busy){
    count := count + 1.U
  }
  val compute = RegInit(0.U(regWidth.W))
  when(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 0.U){
    compute := 0.U
  }.elsewhen(ac_busy){
    compute := compute + 1.U
  }

  val memNum_r = RegInit(0.U(regWidth.W))
  val memNum_w = RegInit(0.U(regWidth.W))

  val missAddr = RegInit(0.U(vaddrBits.W))

  val jk_idle :: jk_compute :: jk_resp :: jk_lmmu_miss :: Nil = Enum(4)
  val jk_state = RegInit(jk_idle)

  mem.io.mmu <> acc.io.mmu2llc


  //一拍的时间接受指令，下一拍的时间返回结果
  //后面可以设置成一个指令fifo
  io.cmd.ready := !canResp
  when(io.cmd.fire){
    canResp := true.B
  }.elsewhen(io.resp.fire){
    canResp := false.B
  }

  rd := io.cmd.bits.inst.rd    //下一拍一定会返回
  io.resp.bits.rd := rd
  io.resp.bits.data := rd_data
  io.resp.valid := canResp

  when(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 1.U){ //查询加速器是否在运行
    rd_data := ac_busy
  }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 2.U){ //查询加速器运行时间
    rd_data := count
  }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 3.U){ //查询加速器对外访存读次数
    rd_data := memNum_r
  }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 4.U){ //查询加速器对外访存写次数
    rd_data := memNum_w
  }.elsewhen(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct === 5.U){ //查询加速器计算时间
    rd_data := compute
  }

  when(acc.io.mmu2llc.Request.fire){
    when(acc.io.mmu2llc.Request.bits.RequestType_isWrite === 0.U){
      memNum_r := memNum_r + 1.U
    }.elsewhen(acc.io.mmu2llc.Request.bits.RequestType_isWrite === 1.U){
      memNum_w := memNum_w + 1.U
    }
  }
  io.interrupt := false.B
  io.badvaddr_ygjk := Mux(jk_state=/=jk_resp, missAddr, missAddr+1.U)
  switch(jk_state){
    is(jk_idle){
      when(io.cmd.fire && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct(5,0) === 0.U){
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
      when(io.cmd.fire && io.cmd.bits.inst.opcode === "h2B".U && io.cmd.bits.inst.funct === 0.U){
        // 收到中断响应
        jk_state := jk_idle
      }
    }
  }
  //opcode对应的是路由到某个加速器用的，CUSTOM0、CUSTOM1、CUSTOM2、CUSTOM3这四组opcode
  //我们这里默认使用opcode为0x0B的指令，将funct的最高位为1的指令作为配置指令。
  acc.io.ctrl2top.config.valid := io.cmd.fire() && io.cmd.bits.inst.opcode === "h0B".U && io.cmd.bits.inst.funct(6) === 1.U
  //输出指令信息,io.cmd.bits.inst.funct
  // printf("funct: %x\n", io.cmd.bits.inst.funct)
  when(io.cmd.fire){
    if (YJPDebugEnable)
    {
      printf("CUTE: opcode: %x, rs1: %x, rs2: %x, rd: %x, funct: %x\n", io.cmd.bits.inst.opcode, io.cmd.bits.rs1, io.cmd.bits.rs2, io.cmd.bits.inst.rd, io.cmd.bits.inst.funct)
    }
  }
  acc.io.ctrl2top.config.bits.cfgData1 := io.cmd.bits.rs1
  acc.io.ctrl2top.config.bits.cfgData2 := io.cmd.bits.rs2
  acc.io.ctrl2top.config.bits.func := io.cmd.bits.inst.funct
  acc.io.ctrl2top.reset := false.B  //多次重启时置位，未实现

}
