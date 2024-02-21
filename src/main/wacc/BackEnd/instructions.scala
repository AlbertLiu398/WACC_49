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

    case class Content(reg : Register, offset: ImmVal) extends Operand {
        override def getValue(): String = { 
            if (offset.value != 0) return s"[${reg.getValue()}, ${offset.getValue()}]"
            else return s"[${reg.getValue()}]"
        }
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

    case class zeroReg(n: Int) extends Register {
        override def getValue(): String = s"xzr"
    }


    // ---------------- Instructions --------------
    sealed trait Instruction {
        def printInstr() : String
    }

    case class I_Add(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"add ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }


    case class I_Sub(dest: Register, src: Register, op: Operand, signed: Boolean=false) extends Instruction {
        override def printInstr(): String = s"sub ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_ReverseSub(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"subr ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Mul(dest1: Register, dest2: Register, src1: Register) extends Instruction {
        override def printInstr(): String = s"mul ${dest1.getValue()}, ${dest2.getValue()}, ${src1.getValue()}"
    }

    case class I_UDiv(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"udiv ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Load(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"ldr ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_LoadSByte(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"ldrb ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_LoadPair(dest1: Register, dest2: Register, op: Operand, op2: Operand) extends Instruction {
        override def printInstr(): String = s"ldp ${dest1.getValue()}, ${dest2.getValue()}, ${op.getValue()}, ${op2.getValue()}"
    }

    case class I_Store(src: Register, dest: Operand) extends Instruction {
        override def printInstr(): String = s"str ${src.getValue()}, ${dest.getValue()}"
    }
    case class I_StorePair(src1: Register, src2: Register, dest: Operand, op: Operand) extends Instruction {
        override def printInstr(): String = s"stp ${src1.getValue()}, ${src2.getValue()}, ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_StoreByte(src: Register, dest:Operand) extends Instruction {
        override def printInstr(): String = s"strb ${src.getValue()}, ${dest.getValue()}"
    }

    case class I_Move(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"mov ${dest.getValue()}, ${op.getValue()}"
    }

    case class I_Branch(label: String) extends Instruction {
        override def printInstr(): String = s"b $label"
    }

    case class I_BranchLink(dstLabel: String) extends Instruction {
        override def printInstr(): String = s"bl $dstLabel"
    }

    case class I_And(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"and ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Xor(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"eor ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Orr(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"orr ${dest.getValue()}, ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Cmp(src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"cmp ${src.getValue()}, ${op.getValue()}"
    }

    case class I_Push(src: Register ) extends Instruction {
        override def printInstr(): String = s"push ${src.getValue()}"
    }

    case class I_Pop(dest: Register) extends Instruction {
        override def printInstr(): String = s"pop ${dest.getValue()}"
    }

    case class I_Label(labelName: String) extends Instruction {
        override def printInstr(): String = s"$labelName:"
    }

    case class I_Directive(name: String) extends Instruction {
        override def printInstr(): String = s".$name"
    }

    case class I_CSet(dest: Register, condition: Conditions) extends Instruction {
        override def printInstr(): String = s"cset${dest.getValue()}, ${condition.toString()}"
    }

    case class I_Neg(dest: Register, src: Register, shift: Shift) extends Instruction {
        override def printInstr(): String = s"neg ${dest.getValue()}, ${src.getValue()}, ${shift.getValue()}"
    }

    case class I_ADRP(dest: Register, label: String) extends Instruction {
        override def printInstr(): String = s"adrp ${dest.getValue()}, $label"
    }

    case object I_Ret extends Instruction {
        override def printInstr(): String = "ret"
    }


}
