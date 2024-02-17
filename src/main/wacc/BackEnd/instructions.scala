package wacc

import java.util.concurrent.locks.Condition
import conditions._
import shift._
object instruction {
    // --------------- Operands -----------------
    sealed trait Operand

    case class ImmVal(value: Int) extends Operand

    case class LabelOp(value: String) extends Operand

    case class ImmValChar(value: Char) extends Operand

    // ----------------- Register -----------------
    sealed trait Register extends Operand

    case class Reg(n: Int) extends Register

    // ---------------- Instructions --------------
    sealed trait Instruction

    case class I_Add(dest: Register, src: Register, op: Operand) extends Instruction

    case class I_Sub(dest: Register, src: Register, op: Operand, signed: Boolean=false) extends Instruction

    case class I_ReverseSub(dest: Register, src: Register, op: Operand) extends Instruction

    case class I_Mul(dest1: Register, dest2: Register, src1: Register) extends Instruction

    case class I_UDiv(dest: Register, src: Register, op: Operand) extends Instruction

    case class I_Load(dest: Register, op: Operand) extends Instruction

    case class I_LoadSByte(dest: Register, op: Operand) extends Instruction

    case class I_Store(src: Register, dest: Operand) extends Instruction

    case class I_StoreByte(src: Register, dest:Operand) extends Instruction

    case class I_Move(dest: Register, op: Operand) extends Instruction

    case class I_Branch(label: String) extends Instruction

    case class I_BranchLink(dstLabel: String) extends Instruction

    case class I_And(dest: Register, src: Register, op: Operand) extends Instruction

    case class I_Xor(dest: Register, src: Register, op: Operand) extends Instruction

    case class I_Orr(dest: Register, src: Register, op: Operand) extends Instruction

    case class I_Cmp(src: Register, op: Operand) extends Instruction

    case class I_Push( src: Register ) extends Instruction

    case class I_I_Pop(dest: Register) extends Instruction

    case class I_I_Label(labelName: String) extends Instruction

    case class I_Directive(name: String) extends Instruction

    case class I_CSet(dest: Register, condition: Conditions) extends Instruction

    case class I_Neg(dest: Register, src: Register, shift: Shift) extends Instruction

}