package wacc

import java.util.concurrent.locks.Condition
import Conditions._
import Shift._
import Constant._

object Instruction {
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
        override def getValue(): String = s"#${value.toInt}"
    }

    case class Op(value: String) extends Operand {
        override def getValue(): String = value
    }

    case class Content(reg : Register, offset: Operand = ImmVal(0), shift: Shifts = LSL(0)) extends Operand {
        override def getValue(): String = { 
            var s = s"[${reg.getValue()}"
            if (offset != ImmVal(0)) {
                s += s", ${offset.getValue()}"
            }
            if (shift != LSL(0)) {
                s += s", ${shift.getValue()}"
            }
            s + "]"
        }
    }

    // ----------------- Register -----------------
    sealed trait Register extends Operand

    case class spReg(n: Int) extends Register{
        var sp = "0"
        override def getValue(): String = s"sp"
    }

    case class Reg(n: Int, size: Int) extends Register {
        override def getValue(): String = {
            n match {
                case 29 => "fp"
                case 30 => "lr"
                case 31 => "sp"
                case _ => {
                    size match {
                        case 32 => s"w$n"
                        case 64 => s"x$n"
                    }
                }
            }
        }

        // Convert to 32 bit register for printing
        def toW(): Register = {
            Reg(n, W_REGISTER_SIZE)
        }
    }

    case class zeroReg(n: Int) extends Register {
        override def getValue(): String = s"xzr"
    }


    // ---------------- Instructions --------------
    sealed trait Instruction {
        def printInstr() : String
    }

    case class I_Add(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      add ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Adds(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      adds ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }


    case class I_Sub(dest: Register, src: Register, op: Operand, signed: Boolean=false) extends Instruction {
        override def printInstr(): String = s"      sub ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Subs(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      subs ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_ReverseSub(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      subr ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Mul(dest1: Register, dest2: Register, src1: Register) extends Instruction {
        override def printInstr(): String = s"      mul ${dest1.getValue()}, ${dest2.getValue()}, ${src1.getValue()} \n"
    }

    case class I_SMul(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      smull ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }


    case class I_SDiv(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      sdiv ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Load(dest: Register, op: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction {
        override def printInstr(): String = 
            op2 match {
                case ImmVal(0) => s"      ldr ${dest.getValue()}, ${op.getValue()}" + (if (update_sp) " !\n" else "\n")
                case _ => s"      ldr ${dest.getValue()},, ${op.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
            }
    }

    case class I_LoadSByte(dest: Register, op: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction {
        override def printInstr(): String = 
            op2 match {
                case ImmVal(0) => s"      ldrb ${dest.getValue()}, ${op.getValue()}" + (if (update_sp) " !\n" else "\n")
                case _ => s"      ldrp ${dest.getValue()},, ${op.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
            }
    }

    case class I_LoadPair(src1: Register, src2: Register, dst: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction {
        override def printInstr(): String = 
            op2 match {
                case ImmVal(0) => s"      ldp ${src1.getValue()}, ${src2.getValue()}, ${dst.getValue()}" + (if (update_sp) "!\n" else "\n")
                case _ => s"      ldp ${src1.getValue()}, ${src2.getValue()}, ${dst.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
            }
    }

    case class I_Store(src: Register, dest: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction {
        override def printInstr(): String = 
            op2 match {
                    case ImmVal(0) => s"      str ${src.getValue()}, ${dest.getValue()}" + (if (update_sp) "!\n" else "\n")
                    case _ => s"      str ${src.getValue()}, ${dest.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                }
    }
    case class I_StorePair(src1: Register, src2: Register, dest: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction {
        override def printInstr(): String = 
            op2 match {
                case ImmVal(0) => s"      stp ${src1.getValue()}, ${src2.getValue()}, ${dest.getValue()}" + (if (update_sp) "!\n" else "\n")
                case _ => s"      stp ${src1.getValue()}, ${src2.getValue()}, ${dest.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
            }
    }

    case class I_StoreByte(src: Register, dest:Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction {
        override def printInstr(): String = 
            op2 match {
                    case ImmVal(0) => s"      strb ${src.getValue()}, ${dest.getValue()}" + (if (update_sp) "!\n" else "\n")
                    case _ => s"      strb ${src.getValue()}, ${dest.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                }
        
    }

    case class I_Move(dest: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      mov ${dest.getValue()}, ${op.getValue()} \n"
    }

    case class I_Branch(label: I_Label, condition: Conditions = null) extends Instruction {
        override def printInstr(): String = {
            condition match {
                case null => return s"      b ${label.labelName} \n"
                case _ =>  return s"      b.$condition ${label.labelName} \n"
            }
        }
    }

    case class I_BranchLink(dstLabel: I_Label) extends Instruction {
        override def printInstr(): String = s"      bl ${dstLabel.labelName} \n"
    }

    case class I_And(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      and ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Xor(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      eor ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Orr(dest: Register, src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      orr ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Cmp(src: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      cmp ${src.getValue()}, ${op.getValue()} \n"
    }

    case class I_Cmp_Shift(src: Register, op: Operand, shift: Shifts) extends Instruction {
        override def printInstr(): String = s"      cmp ${src.getValue()}, ${op.getValue()}, ${shift.getValue()} \n"
    }

    case class I_Push(src: Register ) extends Instruction {
        override def printInstr(): String = s"      push ${src.getValue()} \n"
    }

    case class I_Pop(dest: Register) extends Instruction {
        override def printInstr(): String = s"      pop ${dest.getValue()} \n"
    }

    case class I_Label(labelName: String) extends Instruction  {
        override def printInstr(): String = s"$labelName: \n"
        def getValue(): String = labelName
        
    }

    case class I_Directive(name: String) extends Instruction {
        override def printInstr(): String = s"$name \n"
    }

    case class I_CSet(dest: Register, condition: Conditions) extends Instruction {
        override def printInstr(): String = s"      cset ${dest.getValue()}, ${condition.toString()} \n"
    }

    case class I_Neg(dest: Register, src: Register, shift: Shifts) extends Instruction {
        override def printInstr(): String = s"      neg ${dest.getValue()}, ${src.getValue()}, ${shift.getValue()} \n"
    }

    case class I_ADRP(dest: Register, label: I_Label) extends Instruction {
       override def printInstr(): String = s"      adrp ${dest.getValue()}, ${label.labelName} \n"
    }
    case class I_ADR(dest: Register, label: I_Label) extends Instruction {
        override def printInstr(): String = s"      adr ${dest.getValue()}, ${label.labelName} \n"
    }

    case object I_Ret extends Instruction {
        override def printInstr(): String = "      ret \n"
    }

    case class I_Cbz(reg: Register, label: I_Label) extends Instruction {
        override def printInstr(): String = s"      cbz ${reg.getValue()}, ${label.labelName} \n"

    }

   case class I_Ldrsw(reg1: Register, content: Content) extends Instruction {
        override def printInstr(): String = s"      ldrsw ${reg1.getValue()}, ${content.getValue()} \n"
    }

    case class I_Csel(reg1: Register, reg2: Register, reg3: Register, condition: Conditions) extends Instruction {
        override def printInstr(): String = s"      csel ${reg1.getValue()}, ${reg2.getValue()}, ${reg3.getValue()}, ${condition.toString()} \n"
    }

    case class I_Tst(reg1: Register, op: Operand) extends Instruction {
        override def printInstr(): String = s"      tst ${reg1.getValue()}, ${op.getValue()} \n"
    }

    case class I_Sbfx(reg1: Register, reg2: Register, start_bit: Operand, len: Operand) extends Instruction {
        override def printInstr(): String = s"      sbfx ${reg1.getValue()}, ${reg2.getValue()}, ${start_bit.getValue()}, ${len.getValue()} \n"
    }
}
