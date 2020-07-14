package xiangshan.backend.decode.isa

import chisel3._
import chisel3.util._
import xiangshan.{FuType, HasXSParameter}
import xiangshan.backend.decode._
import xiangshan.backend._

object RV32I_ALUInstr extends HasInstrType with HasXSParameter {
  def ADDI    = BitPat("b????????????_?????_000_?????_0010011")
  def SLLI    = if (XLEN == 32) BitPat("b0000000?????_?????_001_?????_0010011")
                           else BitPat("b000000??????_?????_001_?????_0010011")
  def SLTI    = BitPat("b????????????_?????_010_?????_0010011")
  def SLTIU   = BitPat("b????????????_?????_011_?????_0010011")
  def XORI    = BitPat("b????????????_?????_100_?????_0010011")
  def SRLI    = if (XLEN == 32) BitPat("b0000000?????_?????_101_?????_0010011")
                           else BitPat("b000000??????_?????_101_?????_0010011")
  def ORI     = BitPat("b????????????_?????_110_?????_0010011")
  def ANDI    = BitPat("b????????????_?????_111_?????_0010011")
  def SRAI    = if (XLEN == 32) BitPat("b0100000?????_?????_101_?????_0010011")
                           else BitPat("b010000??????_?????_101_?????_0010011")

  def ADD     = BitPat("b0000000_?????_?????_000_?????_0110011")
  def SLL     = BitPat("b0000000_?????_?????_001_?????_0110011")
  def SLT     = BitPat("b0000000_?????_?????_010_?????_0110011")
  def SLTU    = BitPat("b0000000_?????_?????_011_?????_0110011")
  def XOR     = BitPat("b0000000_?????_?????_100_?????_0110011")
  def SRL     = BitPat("b0000000_?????_?????_101_?????_0110011")
  def OR      = BitPat("b0000000_?????_?????_110_?????_0110011")
  def AND     = BitPat("b0000000_?????_?????_111_?????_0110011")
  def SUB     = BitPat("b0100000_?????_?????_000_?????_0110011")
  def SRA     = BitPat("b0100000_?????_?????_101_?????_0110011")

  def AUIPC   = BitPat("b????????????????????_?????_0010111")
  def LUI     = BitPat("b????????????????????_?????_0110111")

  val table = Array(
    ADDI           -> List(InstrI, FuType.alu, ALUOpType.add),
    SLLI           -> List(InstrI, FuType.alu, ALUOpType.sll),
    SLTI           -> List(InstrI, FuType.alu, ALUOpType.slt),
    SLTIU          -> List(InstrI, FuType.alu, ALUOpType.sltu),
    XORI           -> List(InstrI, FuType.alu, ALUOpType.xor),
    SRLI           -> List(InstrI, FuType.alu, ALUOpType.srl),
    ORI            -> List(InstrI, FuType.alu, ALUOpType.or ),
    ANDI           -> List(InstrI, FuType.alu, ALUOpType.and),
    SRAI           -> List(InstrI, FuType.alu, ALUOpType.sra),

    ADD            -> List(InstrR, FuType.alu, ALUOpType.add),
    SLL            -> List(InstrR, FuType.alu, ALUOpType.sll),
    SLT            -> List(InstrR, FuType.alu, ALUOpType.slt),
    SLTU           -> List(InstrR, FuType.alu, ALUOpType.sltu),
    XOR            -> List(InstrR, FuType.alu, ALUOpType.xor),
    SRL            -> List(InstrR, FuType.alu, ALUOpType.srl),
    OR             -> List(InstrR, FuType.alu, ALUOpType.or ),
    AND            -> List(InstrR, FuType.alu, ALUOpType.and),
    SUB            -> List(InstrR, FuType.alu, ALUOpType.sub),
    SRA            -> List(InstrR, FuType.alu, ALUOpType.sra),

    AUIPC          -> List(InstrU, FuType.alu, ALUOpType.add),
    LUI            -> List(InstrU, FuType.alu, ALUOpType.add)
  )
}

object RV32I_BRUInstr extends HasInstrType {
  def JAL     = BitPat("b????????????????????_?????_1101111")
  def JALR    = BitPat("b????????????_?????_000_?????_1100111")

  def BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  def BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  def BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  def BGE     = BitPat("b???????_?????_?????_101_?????_1100011")
  def BLTU    = BitPat("b???????_?????_?????_110_?????_1100011")
  def BGEU    = BitPat("b???????_?????_?????_111_?????_1100011")

  val table = Array(
    JAL            -> List(InstrJ, FuType.jmp, JumpOpType.jal),
    JALR           -> List(InstrI, FuType.jmp, JumpOpType.jalr),

    BEQ            -> List(InstrB, FuType.alu, ALUOpType.beq),
    BNE            -> List(InstrB, FuType.alu, ALUOpType.bne),
    BLT            -> List(InstrB, FuType.alu, ALUOpType.blt),
    BGE            -> List(InstrB, FuType.alu, ALUOpType.bge),
    BLTU           -> List(InstrB, FuType.alu, ALUOpType.bltu),
    BGEU           -> List(InstrB, FuType.alu, ALUOpType.bgeu)
  )

  val bruFuncTobtbTypeTable = List(
    ALUOpType.beq  -> BTBtype.B,
    ALUOpType.bne  -> BTBtype.B,
    ALUOpType.blt  -> BTBtype.B,
    ALUOpType.bge  -> BTBtype.B,
    ALUOpType.bltu -> BTBtype.B,
    ALUOpType.bgeu -> BTBtype.B,
//    ALUOpType.call -> BTBtype.J,
//    ALUOpType.ret  -> BTBtype.R,
    JumpOpType.jal  -> BTBtype.J,
    JumpOpType.jalr -> BTBtype.I
  )
}

object RV32I_LSUInstr extends HasInstrType {
  def LB      = BitPat("b????????????_?????_000_?????_0000011")
  def LH      = BitPat("b????????????_?????_001_?????_0000011")
  def LW      = BitPat("b????????????_?????_010_?????_0000011")
  def LBU     = BitPat("b????????????_?????_100_?????_0000011")
  def LHU     = BitPat("b????????????_?????_101_?????_0000011")
  def SB      = BitPat("b???????_?????_?????_000_?????_0100011")
  def SH      = BitPat("b???????_?????_?????_001_?????_0100011")
  def SW      = BitPat("b???????_?????_?????_010_?????_0100011")

  val table = Array(
    LB             -> List(InstrI, FuType.ldu, LSUOpType.lb ),
    LH             -> List(InstrI, FuType.ldu, LSUOpType.lh ),
    LW             -> List(InstrI, FuType.ldu, LSUOpType.lw ),
    LBU            -> List(InstrI, FuType.ldu, LSUOpType.lbu),
    LHU            -> List(InstrI, FuType.ldu, LSUOpType.lhu),

    SB             -> List(InstrS, FuType.stu, LSUOpType.sb ),
    SH             -> List(InstrS, FuType.stu, LSUOpType.sh ),
    SW             -> List(InstrS, FuType.stu, LSUOpType.sw )
  )
}

object RV64IInstr extends HasInstrType {
  def ADDIW   = BitPat("b???????_?????_?????_000_?????_0011011")
  def SLLIW   = BitPat("b0000000_?????_?????_001_?????_0011011")
  def SRLIW   = BitPat("b0000000_?????_?????_101_?????_0011011")
  def SRAIW   = BitPat("b0100000_?????_?????_101_?????_0011011")
  def SLLW    = BitPat("b0000000_?????_?????_001_?????_0111011")
  def SRLW    = BitPat("b0000000_?????_?????_101_?????_0111011")
  def SRAW    = BitPat("b0100000_?????_?????_101_?????_0111011")
  def ADDW    = BitPat("b0000000_?????_?????_000_?????_0111011")
  def SUBW    = BitPat("b0100000_?????_?????_000_?????_0111011")

  def LWU     = BitPat("b???????_?????_?????_110_?????_0000011")
  def LD      = BitPat("b???????_?????_?????_011_?????_0000011")
  def SD      = BitPat("b???????_?????_?????_011_?????_0100011")

  val table = Array(
    ADDIW          -> List(InstrI, FuType.alu, ALUOpType.addw),
    SLLIW          -> List(InstrI, FuType.alu, ALUOpType.sllw),
    SRLIW          -> List(InstrI, FuType.alu, ALUOpType.srlw),
    SRAIW          -> List(InstrI, FuType.alu, ALUOpType.sraw),
    SLLW           -> List(InstrR, FuType.alu, ALUOpType.sllw),
    SRLW           -> List(InstrR, FuType.alu, ALUOpType.srlw),
    SRAW           -> List(InstrR, FuType.alu, ALUOpType.sraw),
    ADDW           -> List(InstrR, FuType.alu, ALUOpType.addw),
    SUBW           -> List(InstrR, FuType.alu, ALUOpType.subw),

    LWU            -> List(InstrI, FuType.ldu, LSUOpType.lwu),
    LD             -> List(InstrI, FuType.ldu, LSUOpType.ld ),
    SD             -> List(InstrS, FuType.stu, LSUOpType.sd)
  )
}

object RVIInstr extends HasXSParameter {
  val table = RV32I_ALUInstr.table ++ RV32I_BRUInstr.table ++ RV32I_LSUInstr.table ++
    (if (XLEN == 64) RV64IInstr.table else Nil)
}
