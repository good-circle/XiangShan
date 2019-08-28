package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object ALUOpType {
  def add  = "b00000".U
  def sll  = "b00001".U
  def slt  = "b00010".U
  def sltu = "b00011".U
  def xor  = "b00100".U
  def srl  = "b00101".U
  def or   = "b00110".U
  def and  = "b00111".U
  def sub  = "b01000".U
  def sra  = "b01101".U
  def lui  = "b01111".U
}

object ALUInstr extends HasInstrType {
  def ADDI    = BitPat("b????????????_?????_000_?????_0010011")
  def SLLI    = BitPat("b0000000?????_?????_001_?????_0010011")
  def SLTI    = BitPat("b????????????_?????_010_?????_0010011")
  def SLTIU   = BitPat("b????????????_?????_011_?????_0010011")
  def XORI    = BitPat("b????????????_?????_100_?????_0010011")
  def SRLI    = BitPat("b0000000?????_?????_101_?????_0010011")
  def ORI     = BitPat("b????????????_?????_110_?????_0010011")
  def ANDI    = BitPat("b????????????_?????_111_?????_0010011")
  def SRAI    = BitPat("b0100000?????_?????_101_?????_0010011")

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
    LUI            -> List(InstrU, FuType.alu, ALUOpType.lui)
  )
}

class ALUIO extends FunctionUnitIO {
  val pc = Input(UInt(32.W))
  val npc = Input(UInt(32.W))
  val offset = Input(UInt(32.W))
  val branch = new BranchIO
}

class ALU extends Module {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isAdderSub = (func =/= ALUOpType.add) && !BRUOpType.isJump(func)
  val adderRes = (src1 +& (src2 ^ Fill(32, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(32)
  val slt = xorRes(31) ^ sltu

  val shamt = src2(4, 0)
  val aluRes = LookupTree(func, 0.U, List(
    BRUOpType.jal  -> adderRes,
    BRUOpType.jalr -> adderRes,
    ALUOpType.add  -> adderRes,
    ALUOpType.sll  -> ((src1  << shamt)(31, 0)),
    ALUOpType.slt  -> Cat(0.U(31.W), slt),
    ALUOpType.sltu -> Cat(0.U(31.W), sltu),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (src1  >> shamt),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sub  -> adderRes,
    ALUOpType.lui  -> src2,
    ALUOpType.sra  -> ((src1.asSInt >> shamt).asUInt)
  ))

  val branchOpTable = List(
    BRUOpType.getBranchType(BRUOpType.beq)  -> !xorRes.orR,
    BRUOpType.getBranchType(BRUOpType.blt)  -> slt,
    BRUOpType.getBranchType(BRUOpType.bltu) -> sltu
  )

  val isBranch = BRUOpType.isBranch(func)
  val isBru = BRUOpType.isBru(func)
  val taken = LookupTree(BRUOpType.getBranchType(func), false.B, branchOpTable) ^ BRUOpType.isBranchInvert(func)
  val target = Mux(isBranch, io.pc + io.offset, adderRes)
  io.branch.target := Mux(!taken && isBranch, io.pc + 4.U, target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.branch.isTaken := valid && isBru && (io.branch.target =/= io.npc)
  // may be can move to ISU to calculate pc + 4
  io.out.bits := Mux(isBru, io.pc + 4.U, aluRes)

  io.in.ready := true.B
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid && isBru
  bpuUpdateReq.pc := io.pc
  bpuUpdateReq.isMissPredict := io.branch.target =/= io.npc
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, BRUInstr.bruFuncTobtbTypeTable)

  BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")

  val right = valid && isBru && (io.npc === io.branch.target)
  val wrong = valid && isBru && (io.npc =/= io.branch.target)
  BoringUtils.addSource(right && isBranch, "MbpBRight")
  BoringUtils.addSource(wrong && isBranch, "MbpBWrong")
  BoringUtils.addSource(right && (func === BRUOpType.jal || func === BRUOpType.call), "MbpJRight")
  BoringUtils.addSource(wrong && (func === BRUOpType.jal || func === BRUOpType.call), "MbpJWrong")
  BoringUtils.addSource(right && func === BRUOpType.jalr, "MbpIRight")
  BoringUtils.addSource(wrong && func === BRUOpType.jalr, "MbpIWrong")
  BoringUtils.addSource(right && func === BRUOpType.ret, "MbpRRight")
  BoringUtils.addSource(wrong && func === BRUOpType.ret, "MbpRWrong")
}