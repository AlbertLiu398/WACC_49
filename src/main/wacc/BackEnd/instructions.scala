package wacc

import java.util.concurrent.locks.Condition
import conditions._
import shift._
object instruction {
    // --------------- Operands -----------------
    sealed trait Operand {
        def getValue() : String
    }

    case class ImmVal(value: Int) extends Operand {
        override def getValue(): String = s"#$value"
    }

    case class LabelOp(value: String) extends Operand {
        override def getValue(): String = s"=$value"
    }

    case class ImmValChar(value: Char) extends Operand {
        override def getValue(): String = s"$value"
    }

    // ----------------- Register -----------------
    sealed trait Register extends Operand

    case class spReg(n: Int) extends Register{
        var sp = "0"
        override def getValue(): String = s"x$n"
    }

    case class Reg(n: Int) extends Register {
        override def getValue(): String = s"x$n"
    }


    // ---------------- Instructions --------------
    sealed trait Instruction {
        def printInstr() : String
    }

    case class I_Add(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"ADD ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }


    case class I_Sub(dest: Register, src: Register, op: Operand, signed: Boolean=false) extends Instruction {
        override def printInstr(): String = s"SUB ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_ReverseSub(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"SUBR ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Mul(dest1: Register, dest2: Register, src1: Register) extends Instruction {
        override def printInstr(): String = s"MUL ${dest1.getValue()}, ${dest2.getValue()}, ${src1.getValue()}"
    }

    case class I_UDiv(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"UDIV ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Load(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"LDR ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_LoadSByte(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"LDRB ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_Store(src: Register, dest: Operand) extends Instruction {
        override def printInstr(): String = s"STR ${src.getValue()}, ${dest.getValue()}"
    }

    case class I_StoreByte(src: Register, dest:Operand) extends Instruction {
        override def printInstr(): String = s"STRB ${src.getValue()}, ${dest.getValue()}"
    }

    case class I_Move(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"MOV ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_Branch(label: String) extends Instruction {
        override def printInstr(): String = s"B $label"
    }

    case class I_BranchLink(dstLabel: String) extends Instruction {
        override def printInstr(): String = s"BL $dstLabel"
    }

    case class I_And(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"AND ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Xor(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"EOR ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Orr(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"ORR ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Cmp(src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"CMP ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Push(src: Register ) extends Instruction {
        override def printInstr(): String = s"PUSH ${src.getValue()}"
    }

    case class I_Pop(dest: Register) extends Instruction {
        override def printInstr(): String = s"POP ${dest.getValue()}"
    }

    case class I_Label(labelName: String) extends Instruction {
        override def printInstr(): String = s"$labelName:"
    }

    case class I_Directive(name: String) extends Instruction {
        override def printInstr(): String = s".$name"
    }

    case class I_CSet(dest: Register, condition: Conditions) extends Instruction {
        override def printInstr(): String = s"CSET ${dest.getValue()}, ${condition.toString()}"
    }

    case class I_Neg(dest: Register, src: Register, shift: Shift) extends Instruction {
        override def printInstr(): String = s"NEG ${dest.getValue()}, ${src.getValue()}, ${shift.getValue()}"
    }

    case object I_Ret extends Instruction {
        override def printInstr(): String = "ret"
    }

}
