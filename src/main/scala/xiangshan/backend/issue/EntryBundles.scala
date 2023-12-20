package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.{MathUtils, OptionWrapper}
import utility.HasCircularQueuePtrHelper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{MemWaitUpdateReq, SqPtr, LqPtr}

object EntryBundles extends HasCircularQueuePtrHelper {

  class Status(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    //basic status
    val robIdx                = new RobPtr
    val fuType                = IQFuType()
    //src status
    val srcStatus             = Vec(params.numRegSrc, new SrcStatus)
    //issue status
    val blocked               = Bool()
    val issued                = Bool()
    val firstIssue            = Bool()
    val issueTimer            = UInt(2.W)
    val deqPortIdx            = UInt(1.W)
    //mem status
    val mem                   = OptionWrapper(params.isMemAddrIQ, new StatusMemPart)
    //vector mem status
    val vecMem                = OptionWrapper(params.isVecMemIQ, new StatusVecMemPart)

    def srcReady: Bool        = {
      VecInit(srcStatus.map(_.srcState).map(SrcState.isReady)).asUInt.andR
    }

    def canIssue: Bool        = {
      srcReady && !issued && !blocked
    }

    def mergedLoadDependency: Option[Vec[UInt]] = {
      OptionWrapper(params.hasIQWakeUp, srcStatus.map(_.srcLoadDependency.get).reduce({
        case (l: Vec[UInt], r: Vec[UInt]) => VecInit(l.zip(r).map(x => x._1 | x._2))
      }: (Vec[UInt], Vec[UInt]) => Vec[UInt]))
    }
  }

  class SrcStatus(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val psrc                  = UInt(params.rdPregIdxWidth.W)
    val srcType               = SrcType()
    val srcState              = SrcState()
    val dataSources           = DataSource()
    val srcTimer              = OptionWrapper(params.hasIQWakeUp, UInt(3.W))
    val srcWakeUpL1ExuOH      = OptionWrapper(params.hasIQWakeUp, ExuVec())
    val srcLoadDependency     = OptionWrapper(params.hasIQWakeUp, Vec(LoadPipelineWidth, UInt(3.W)))
  }

  class StatusMemPart(implicit p:Parameters) extends Bundle {
    val waitForSqIdx          = new SqPtr // generated by store data valid check
    val waitForRobIdx         = new RobPtr // generated by store set
    val waitForStd            = Bool()
    val strictWait            = Bool()
    val sqIdx                 = new SqPtr
  }

  class StatusVecMemPart(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
    val sqIdx = new SqPtr
    val lqIdx = new LqPtr
    val uopIdx = UopIdx()
  }

  class EntryDeqRespBundle(implicit p: Parameters, params: IssueBlockParams) extends Bundle {
    val robIdx                = new RobPtr
    val respType              = RSFeedbackType() // update credit if needs replay
    val dataInvalidSqIdx      = new SqPtr
    val rfWen                 = Bool()
    val fuType                = FuType()
    val uopIdx                = OptionWrapper(params.isVecMemIQ, Output(UopIdx()))
  }

  class EntryBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val status                = new Status()
    val imm                   = OptionWrapper(params.needImm, UInt((params.deqImmTypesMaxLen).W))
    val payload               = new DynInst()
  }

  class CommonInBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val flush                 = Flipped(ValidIO(new Redirect))
    val enq                   = Flipped(ValidIO(new EntryBundle))
    //wakeup
    val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
    val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
    //cancel
    val og0Cancel             = Input(ExuOH(backendParams.numExu))
    val og1Cancel             = Input(ExuOH(backendParams.numExu))
    val ldCancel              = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    //deq sel
    val deqSel                = Input(Bool())
    val deqPortIdxWrite       = Input(UInt(1.W))
    val issueResp             = Flipped(ValidIO(new EntryDeqRespBundle))
    //trans sel
    val transSel              = Input(Bool())
    // mem only
    val fromMem = OptionWrapper(params.isMemAddrIQ, new Bundle {
      val stIssuePtr          = Input(new SqPtr)
      val memWaitUpdateReq    = Flipped(new MemWaitUpdateReq)
    })
    // vector mem only
    val fromLsq = OptionWrapper(params.isVecMemIQ, new Bundle {
      val sqDeqPtr = Input(new SqPtr)
      val lqDeqPtr = Input(new LqPtr)
    })
  }

  class CommonOutBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    //status
    val valid                 = Output(Bool())
    val canIssue              = Output(Bool())
    val fuType                = Output(FuType())
    val robIdx                = Output(new RobPtr)
    val uopIdx                = OptionWrapper(params.isVecMemIQ, Output(UopIdx()))
    //src
    val dataSource            = Vec(params.numRegSrc, Output(DataSource()))
    val srcWakeUpL1ExuOH      = OptionWrapper(params.hasIQWakeUp, Vec(params.numRegSrc, Output(ExuVec())))
    val srcTimer              = OptionWrapper(params.hasIQWakeUp, Vec(params.numRegSrc, Output(UInt(3.W))))
    //deq
    val isFirstIssue          = Output(Bool())
    val entry                 = ValidIO(new EntryBundle)
    val deqPortIdxRead        = Output(UInt(1.W))
    val issueTimerRead        = Output(UInt(2.W))
    // debug
    val cancel                = OptionWrapper(params.hasIQWakeUp, Output(Bool()))
  }

  class CommonWireBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val validRegNext          = Bool()
    val flushed               = Bool()
    val clear                 = Bool()
    val canIssue              = Bool()
    val enqReady              = Bool()
    val deqSuccess            = Bool()
    val srcWakeup             = Vec(params.numRegSrc, Bool())
    val srcWakeupByWB         = Vec(params.numRegSrc, Bool())
  }

  def CommonWireConnect(common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet        = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    common.flushed            := status.robIdx.needFlush(commonIn.flush)
    common.deqSuccess         := commonIn.issueResp.valid && commonIn.issueResp.bits.respType === RSFeedbackType.fuIdle && !hasIQWakeupGet.srcLoadCancelVec.asUInt.orR
    common.srcWakeup          := common.srcWakeupByWB.zip(hasIQWakeupGet.srcWakeupByIQ).map { case (x, y) => x || y.asUInt.orR }
    common.srcWakeupByWB      := commonIn.wakeUpFromWB.map(bundle => bundle.bits.wakeUp(status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType), bundle.valid)).transpose.map(x => VecInit(x.toSeq).asUInt.orR).toSeq
    common.canIssue           := validReg && status.canIssue && !hasIQWakeupGet.srcCancelVec.asUInt.orR
    common.enqReady           := !validReg || common.clear
    if(isEnq) {
      common.validRegNext     := Mux(commonIn.enq.valid && common.enqReady, true.B, Mux(common.clear, false.B, validReg))
      common.clear            := common.flushed || common.deqSuccess || commonIn.transSel
    } else {
      common.validRegNext     := Mux(commonIn.enq.valid && commonIn.transSel, true.B, Mux(common.clear, false.B, validReg))
      common.clear            := common.flushed || common.deqSuccess
    }
  }

  class CommonIQWakeupBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val srcWakeupByIQ                             = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupByIQWithoutCancel                = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupByIQButCancel                    = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val regSrcWakeupL1ExuOH                       = Vec(params.numRegSrc, ExuVec())
    val srcWakeupL1ExuOHOut                       = Vec(params.numRegSrc, ExuVec())
    val srcLoadDependencyOut                      = Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(3.W)))
    val wakeupLoadDependencyByIQVec               = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
    val shiftedWakeupLoadDependencyByIQVec        = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
    val shiftedWakeupLoadDependencyByIQBypassVec  = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
    val cancelVec                                 = Vec(params.numRegSrc, Bool())
    val srcCancelVec                              = Vec(params.numRegSrc, Bool())
    val srcLoadCancelVec                          = Vec(params.numRegSrc, Bool())
    val canIssueBypass                            = Bool()
  }

  def CommonIQWakeupConnect(common: CommonWireBundle, hasIQWakeupGet: CommonIQWakeupBundle, validReg: Bool, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val wakeupVec: Seq[Seq[Bool]] = commonIn.wakeUpFromIQ.map((bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
      bundle.bits.wakeUpFromIQ(status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType))
    ).toSeq.transpose
    val cancelSel = params.wakeUpSourceExuIdx.zip(commonIn.wakeUpFromIQ).map { case (x, y) => commonIn.og0Cancel(x) && y.bits.is0Lat }

    hasIQWakeupGet.srcCancelVec.zip(hasIQWakeupGet.srcLoadCancelVec).zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zipWithIndex.foreach { case (((srcCancel, srcLoadCancel), wakeUpByIQVec), srcIdx) =>
      val ldTransCancel                             = Mux1H(wakeUpByIQVec, hasIQWakeupGet.wakeupLoadDependencyByIQVec.map(dep => LoadShouldCancel(Some(dep), commonIn.ldCancel)))
      srcLoadCancel                                 := LoadShouldCancel(status.srcStatus(srcIdx).srcLoadDependency, commonIn.ldCancel)
      srcCancel                                     := srcLoadCancel || ldTransCancel
    }
    hasIQWakeupGet.cancelVec                        := hasIQWakeupGet.srcCancelVec
    hasIQWakeupGet.srcWakeupByIQ                    := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
    hasIQWakeupGet.srcWakeupByIQButCancel           := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && cancel }))
    hasIQWakeupGet.srcWakeupByIQWithoutCancel       := wakeupVec.map(x => VecInit(x))
    hasIQWakeupGet.wakeupLoadDependencyByIQVec      := commonIn.wakeUpFromIQ.map(_.bits.loadDependency).toSeq
    hasIQWakeupGet.regSrcWakeupL1ExuOH.zip(status.srcStatus.map(_.srcWakeUpL1ExuOH.get)).foreach {
      case (exuOH, regExuOH) =>
        exuOH                                       := 0.U.asTypeOf(exuOH)
        params.wakeUpSourceExuIdx.foreach(x => exuOH(x) := regExuOH(x))
    }
    hasIQWakeupGet.srcWakeupL1ExuOHOut.zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zip(common.srcWakeup).zipWithIndex.foreach {
      case (((exuOH: Vec[Bool], wakeUpByIQOH: Vec[Bool]), wakeUp: Bool), srcIdx) =>
        if(isEnq) {
          ExuOHGen(exuOH, wakeUpByIQOH, wakeUp, status.srcStatus(srcIdx).srcWakeUpL1ExuOH.get)
        } else {
          ExuOHGen(exuOH, wakeUpByIQOH, wakeUp, hasIQWakeupGet.regSrcWakeupL1ExuOH(srcIdx))
        }
    }
    hasIQWakeupGet.srcLoadDependencyOut.zip(hasIQWakeupGet.srcWakeupByIQ).foreach {
      case (loadDependencyOut, wakeUpByIQVec) =>
        loadDependencyOut                           := Mux1H(wakeUpByIQVec, hasIQWakeupGet.shiftedWakeupLoadDependencyByIQBypassVec)
    }
    hasIQWakeupGet.canIssueBypass                   := validReg && !status.issued && !status.blocked &&
      VecInit(status.srcStatus.map(_.srcState).zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zipWithIndex.map { case ((state, wakeupVec), srcIdx) =>
        val cancel = hasIQWakeupGet.srcCancelVec(srcIdx)
        Mux(cancel, false.B, wakeupVec.asUInt.orR | state)
      }).asUInt.andR
  }


  def ShiftLoadDependency(hasIQWakeupGet: CommonIQWakeupBundle)(implicit p: Parameters, params: IssueBlockParams) = {
    hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec
      .zip(hasIQWakeupGet.wakeupLoadDependencyByIQVec)
      .zip(params.wakeUpInExuSources.map(_.name)).foreach {
      case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
        case ((dep, originalDep), deqPortIdx) =>
          if (name.contains("LDU") && name.replace("LDU", "").toInt == deqPortIdx)
            dep := (originalDep << 2).asUInt | 2.U
          else
            dep := originalDep << 1
      }
    }
    hasIQWakeupGet.shiftedWakeupLoadDependencyByIQBypassVec
      .zip(hasIQWakeupGet.wakeupLoadDependencyByIQVec)
      .zip(params.wakeUpInExuSources.map(_.name)).foreach {
      case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
        case ((dep, originalDep), deqPortIdx) =>
          if (name.contains("LDU") && name.replace("LDU", "").toInt == deqPortIdx)
            dep := (originalDep << 1).asUInt | 1.U
          else
            dep := originalDep
      }
    }
  }

  def EntryRegCommonConnect(common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, entryRegNext: EntryBundle, entryReg: EntryBundle, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet                                 = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    val cancelByLd                                     = hasIQWakeupGet.srcLoadCancelVec.asUInt.orR
    val cancelWhenWakeup                               = VecInit(hasIQWakeupGet.srcWakeupByIQButCancel.map(_.asUInt.orR)).asUInt.orR
    val respIssueFail                                  = commonIn.issueResp.valid && RSFeedbackType.isBlocked(commonIn.issueResp.bits.respType)
    val srcWakeupExuOH                                 = if(isEnq) status.srcStatus.map(_.srcWakeUpL1ExuOH.getOrElse(0.U.asTypeOf(ExuVec()))) else hasIQWakeupGet.regSrcWakeupL1ExuOH
    entryRegNext.status.robIdx                        := status.robIdx
    entryRegNext.status.fuType                        := IQFuType.readFuType(status.fuType, params.getFuCfgs.map(_.fuType))
    entryRegNext.status.srcStatus.zip(status.srcStatus).zipWithIndex.foreach { case ((srcStatusNext, srcStatus), srcIdx) =>
      val cancel = hasIQWakeupGet.srcCancelVec(srcIdx)
      val wakeupByIQ = hasIQWakeupGet.srcWakeupByIQ(srcIdx).asUInt.orR
      val wakeupByIQOH = hasIQWakeupGet.srcWakeupByIQ(srcIdx)
      val wakeup = common.srcWakeup(srcIdx)
      srcStatusNext.psrc                              := srcStatus.psrc
      srcStatusNext.srcType                           := srcStatus.srcType
      srcStatusNext.srcState                          := Mux(cancel, false.B, wakeup | srcStatus.srcState)
      srcStatusNext.dataSources.value                 := Mux(wakeupByIQ, DataSource.bypass, DataSource.reg)
      if(params.hasIQWakeUp) {
        srcStatusNext.srcTimer.get                    := MuxCase(3.U, Seq(
          // T0: waked up by IQ, T1: reset timer as 1
          wakeupByIQ                                  -> 2.U,
          // do not overflow
          srcStatus.srcTimer.get.andR                 -> srcStatus.srcTimer.get,
          // T2+: increase if the entry is valid, the src is ready, and the src is woken up by iq
          (validReg && SrcState.isReady(srcStatus.srcState) && srcWakeupExuOH(srcIdx).asUInt.orR) -> (srcStatus.srcTimer.get + 1.U)
        ))
        ExuOHGen(srcStatusNext.srcWakeUpL1ExuOH.get, wakeupByIQOH, wakeup, srcWakeupExuOH(srcIdx))
        srcStatusNext.srcLoadDependency.get           :=
          Mux(wakeup,
            Mux1H(wakeupByIQOH, hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec),
            Mux(validReg && srcStatus.srcLoadDependency.get.asUInt.orR, VecInit(srcStatus.srcLoadDependency.get.map(i => i(i.getWidth - 2, 0) << 1)), srcStatus.srcLoadDependency.get))
      }
    }
    entryRegNext.status.blocked                       := false.B
    entryRegNext.status.issued                        := MuxCase(status.issued, Seq(
      (cancelByLd || cancelWhenWakeup || respIssueFail) -> false.B,
      commonIn.deqSel                                   -> true.B,
      !status.srcReady                                  -> false.B,
    ))
    entryRegNext.status.firstIssue                    := commonIn.deqSel || status.firstIssue
    entryRegNext.status.issueTimer                    := Mux(commonIn.deqSel, 0.U, Mux(status.issued, status.issueTimer + 1.U, "b10".U))
    entryRegNext.status.deqPortIdx                    := Mux(commonIn.deqSel, commonIn.deqPortIdxWrite, Mux(status.issued, status.deqPortIdx, 0.U))
    entryRegNext.imm.foreach(_                        := entryReg.imm.get)
    entryRegNext.payload                              := entryReg.payload

  }

  def CommonOutConnect(commonOut: CommonOutBundle, common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, entryReg: EntryBundle, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet                                 = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    val srcWakeupExuOH                                 = if(isEnq) status.srcStatus.map(_.srcWakeUpL1ExuOH.getOrElse(0.U.asTypeOf(ExuVec()))) else hasIQWakeupGet.regSrcWakeupL1ExuOH
    commonOut.valid                                   := validReg
    commonOut.canIssue                                := (common.canIssue || hasIQWakeupGet.canIssueBypass) && !common.flushed
    commonOut.fuType                                  := IQFuType.readFuType(status.fuType, params.getFuCfgs.map(_.fuType)).asUInt
    commonOut.robIdx                                  := status.robIdx
    commonOut.dataSource.zipWithIndex.foreach{ case (dataSourceOut, srcIdx) =>
      dataSourceOut.value                             := Mux(hasIQWakeupGet.srcWakeupByIQ(srcIdx).asUInt.orR, DataSource.forward, status.srcStatus(srcIdx).dataSources.value)
    }
    commonOut.isFirstIssue                            := !status.firstIssue
    commonOut.entry.valid                             := validReg
    commonOut.entry.bits                              := entryReg
    if(isEnq) {
      commonOut.entry.bits.status                     := status
    }
    commonOut.issueTimerRead                          := status.issueTimer
    commonOut.deqPortIdxRead                          := status.deqPortIdx
    if(params.hasIQWakeUp) {
      commonOut.srcWakeUpL1ExuOH.get                  := Mux(hasIQWakeupGet.canIssueBypass && !common.canIssue, hasIQWakeupGet.srcWakeupL1ExuOHOut, VecInit(srcWakeupExuOH))
      commonOut.srcTimer.get.zipWithIndex.foreach { case (srcTimerOut, srcIdx) =>
        val wakeupByIQOH                               = hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx)
        srcTimerOut                                   := Mux(wakeupByIQOH.asUInt.orR, Mux1H(wakeupByIQOH, commonIn.wakeUpFromIQ.map(_.bits.is0Lat).toSeq).asUInt, status.srcStatus(srcIdx).srcTimer.get)
      }
      commonOut.entry.bits.status.srcStatus.map(_.srcLoadDependency.get).zipWithIndex.foreach { case (srcLoadDependencyOut, srcIdx) =>
        srcLoadDependencyOut                          := Mux(hasIQWakeupGet.canIssueBypass && !common.canIssue, hasIQWakeupGet.srcLoadDependencyOut(srcIdx), status.srcStatus(srcIdx).srcLoadDependency.get)
      }
    }
    commonOut.cancel.foreach(_                        := hasIQWakeupGet.cancelVec.asUInt.orR)
    if (params.isVecMemIQ) {
      commonOut.uopIdx.get                            := status.vecMem.get.uopIdx
    }
  }

  def EntryMemConnect(commonIn: CommonInBundle, common: CommonWireBundle, validReg: Bool, entryReg: EntryBundle, entryRegNext: EntryBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val enqValid                                       = if(isEnq) commonIn.enq.valid && (!validReg || common.clear) else commonIn.enq.valid && commonIn.transSel
    val fromMem                                        = commonIn.fromMem.get
    val memStatus                                      = entryReg.status.mem.get
    val memStatusNext                                  = entryRegNext.status.mem.get

    // load cannot be issued before older store, unless meet some condition
    val blockedByOlderStore = isAfter(memStatusNext.sqIdx, fromMem.stIssuePtr)

    val deqFailedForStdInvalid                         = commonIn.issueResp.valid && commonIn.issueResp.bits.respType === RSFeedbackType.dataInvalid

    val staWaitedReleased = Cat(
      fromMem.memWaitUpdateReq.robIdx.map(x => x.valid && x.bits.value === memStatusNext.waitForRobIdx.value)
    ).orR
    val stdWaitedReleased = Cat(
      fromMem.memWaitUpdateReq.sqIdx.map(x => x.valid && x.bits.value === memStatusNext.waitForSqIdx.value)
    ).orR
    val olderStaNotViolate                             = staWaitedReleased && !memStatusNext.strictWait
    val olderStdReady                                  = stdWaitedReleased && memStatusNext.waitForStd
    val waitStd                                        = !olderStdReady
    val waitSta                                        = !olderStaNotViolate

    when(enqValid) {
      memStatusNext.waitForSqIdx                       := commonIn.enq.bits.status.mem.get.waitForSqIdx
      // update by lfst at dispatch stage
      memStatusNext.waitForRobIdx                      := commonIn.enq.bits.status.mem.get.waitForRobIdx
      // new load inst don't known if it is blocked by store data ahead of it
      memStatusNext.waitForStd                         := false.B
      // update by ssit at rename stage
      memStatusNext.strictWait                         := commonIn.enq.bits.status.mem.get.strictWait
      memStatusNext.sqIdx                              := commonIn.enq.bits.status.mem.get.sqIdx
    }.elsewhen(deqFailedForStdInvalid) {
      // Todo: check if need assign statusNext.block
      memStatusNext.waitForSqIdx                       := commonIn.issueResp.bits.dataInvalidSqIdx
      memStatusNext.waitForRobIdx                      := memStatus.waitForRobIdx
      memStatusNext.waitForStd                         := true.B
      memStatusNext.strictWait                         := memStatus.strictWait
      memStatusNext.sqIdx                              := memStatus.sqIdx
    }.otherwise {
      memStatusNext                                    := memStatus
    }

    val shouldBlock = Mux(commonIn.enq.valid && commonIn.transSel, commonIn.enq.bits.status.blocked, entryReg.status.blocked)
    val blockNotReleased = waitStd || waitSta
    val respBlock = deqFailedForStdInvalid
    entryRegNext.status.blocked := shouldBlock && blockNotReleased && blockedByOlderStore || respBlock
    shouldBlock && blockNotReleased && blockedByOlderStore || respBlock
  }

  def ExuOHGen(exuOH: Vec[Bool], wakeupByIQOH: Vec[Bool], wakeup: Bool, regSrcExuOH: Vec[Bool])(implicit p: Parameters, params: IssueBlockParams) = {
    val origExuOH = 0.U.asTypeOf(exuOH)
    when(wakeupByIQOH.asUInt.orR) {
      origExuOH := Mux1H(wakeupByIQOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(p(XSCoreParamsKey).backendParams.numExu.W)).toSeq).asBools
    }.elsewhen(wakeup) {
      origExuOH := 0.U.asTypeOf(origExuOH)
    }.otherwise {
      origExuOH := regSrcExuOH
    }
    exuOH := 0.U.asTypeOf(exuOH)
    params.wakeUpSourceExuIdx.foreach(x => exuOH(x) := origExuOH(x))
  }

  object IQFuType {
    def num = FuType.num

    def apply() = Vec(num, Bool())

    def readFuType(fuType: Vec[Bool], fus: Seq[FuType.OHType]): Vec[Bool] = {
      val res = 0.U.asTypeOf(fuType)
      fus.foreach(x => res(x.id) := fuType(x.id))
      res
    }
  }
}
