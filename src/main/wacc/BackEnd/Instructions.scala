package wacc

import java.util.concurrent.locks.Condition
import Conditions._
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
    }

    case class I_Add(dest: Register, src: Register, op: Operand, updateFlag: Boolean = false) extends Instruction 

    case class I_Sub(dest: Register, src: Register, op: Operand, updateFlag: Boolean=false) extends Instruction 

    case class I_Mul(dest1: Register, dest2: Register, src1: Register, signed: Boolean = false) extends Instruction 

    case class I_SDiv(dest: Register, src: Register, op: Operand) extends Instruction 
        
    case class I_Load(dest: Register, op: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false, load_byte: Boolean = false) extends Instruction 
    case class I_LoadPair(src1: Register, src2: Register, dst: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction 
        
    case class I_Store(src: Register, dest: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false, store_byte: Boolean = false) extends Instruction   
    case class I_StorePair(src1: Register, src2: Register, dest: Operand, op2: Operand = ImmVal(0), update_sp: Boolean = false) extends Instruction

    case class I_Move(dest: Register, op: Operand) extends Instruction 
    case class I_Movk(dest: Register, op: Operand, shift: Shifts) extends Instruction 

    case class I_Branch(label: I_Label, condition: Conditions = null) extends Instruction 
    case class I_BranchLink(dstLabel: I_Label) extends Instruction 

    case class I_And(dest: Register, src: Register, op: Operand) extends Instruction 
    case class I_Xor(dest: Register, src: Register, op: Operand) extends Instruction 
    case class I_Orr(dest: Register, src: Register, op: Operand) extends Instruction 
    case class I_Neg(dest: Register, src: Register, shift: Shifts) extends Instruction 
    
    case class I_Cmp(src: Register, op: Operand) extends Instruction 
    case class I_Cmp_Shift(src: Register, op: Operand, shift: Shifts) extends Instruction 

    case class I_Push(src: Register ) extends Instruction 
    case class I_Pop(dest: Register) extends Instruction 

    case class I_Directive(name: String) extends Instruction
    case class I_Label(labelName: String) extends Instruction  {
        def getValue(): String = labelName
    }

    case class I_CSet(dest: Register, condition: Conditions) extends Instruction 
    case class I_Csel(reg1: Register, reg2: Register, reg3: Register, condition: Conditions) extends Instruction 


    case class I_ADRP(dest: Register, label: I_Label) extends Instruction 
    case class I_ADR(dest: Register, label: I_Label) extends Instruction 

    case object I_Ret extends Instruction

    case class I_Cbz(reg: Register, label: I_Label) extends Instruction 

    case class I_Ldrsw(reg1: Register, content: Content) extends Instruction 


    case class I_Tst(reg1: Register, op: Operand) extends Instruction 

    case class I_Sbfx(reg1: Register, reg2: Register, start_bit: Operand, len: Operand) extends Instruction 

    case class I_Sxtw(reg1: Register, reg2: Register) extends Instruction 


    // ---------Shifts----------------
    sealed trait Shifts extends Operand{}

    case class LSL(value: Int) extends Shifts {
        override def getValue(): String = s"lsl #$value"
    }
    case class LSR(value: Int) extends Shifts{
        override def getValue(): String = s"lsr #$value"
    }
    case class ASR(value: Int) extends Shifts{
        override def getValue(): String = s"asr #$value"
    }
    case class ROR(value: Int) extends Shifts{
        override def getValue(): String = s"ror #$value"
    }
}

