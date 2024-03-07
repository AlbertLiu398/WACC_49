package wacc
import Instruction._

object ArmAssemblyWriter extends AssemblyWriter {


  def printInstr1(name:String, op1:Operand): String = {
    s"      $name ${op1.getValue()} \n"
  }

  def printInstr2(name:String, op1:Operand, op2:Operand): String = {
    s"      $name ${op1.getValue()}, ${op2.getValue()} \n"
  }

  def printInstr2withLabel(name:String, op1:Operand, label:I_Label): String = {
    s"      $name ${op1.getValue()}, ${label.labelName} \n"
  }

  def printInstr3(name:String, op1:Operand, op2:Operand, op3:Operand): String = {
    s"      $name ${op1.getValue()}, ${op2.getValue()}, ${op3.getValue()} \n"
  }

  def printInstr4(name:String, op1:Operand, op2:Operand, op3:Operand, op4:Operand): String = {
    s"      $name ${op1.getValue()}, ${op2.getValue()}, ${op3.getValue()}, ${op4.getValue()} \n"
  }

  def addExclamation(instr: String, update_sp: Boolean): String = {
    if (update_sp) {
      instr.dropRight(1) + (if (update_sp) "!\n" else "\n")
    } else {
      instr
    }
  } 

  override def translateInstruction(instr: Instruction): String = {
    instr match {

        case I_Add(dest, src, op, update_flag) => update_flag match {
                        case false => printInstr3("add", dest, src, op)
                        case true => printInstr3("adds", dest, src, op)
                    }
        case I_Sub(dest, src, op, update_flag) => update_flag match {
                        case false => printInstr3("sub", dest, src, op)
                        case true => printInstr3("subs", dest, src, op)
                    }
        case I_Mul(dest, src, op, signed) => signed match {
                        case false => printInstr3("mul", dest, src, op)
                        case true => printInstr3("smull", dest, src, op)
                    }

        case I_SDiv(dest, src, op) => printInstr3("sdiv", dest, src, op)
        
        case I_Load(dest, op, op2, update_sp, load_byte) =>  op2 match {
                        case ImmVal(0) => 
                            val name = if (load_byte) "ldrb" else "ldr"
                            addExclamation(printInstr2(name, dest, op), update_sp)
                        case _ => 
                            val name = if (load_byte) "ldrb" else "ldr"
                            addExclamation(printInstr3(name, dest, op, op2), update_sp)
                    }  
       
        case I_LoadPair(src1, src2, dst, op2, update_sp) => op2 match {
                        case ImmVal(0) => addExclamation(printInstr3("ldp", src1, src2, dst), update_sp)
                        case _ => addExclamation(printInstr4("ldp", src1, src2, dst, op2), update_sp)
                    }
        
        case I_Store(src, op, op2, update_sp, store_byte) =>  op2 match {
                        case ImmVal(0) => 
                            val name = if (store_byte) "strb" else "str"
                            addExclamation(printInstr2(name, src, op), update_sp)
                        case _ => 
                            val name = if (store_byte) "strb" else "str"
                            addExclamation(printInstr3(name, src, op, op2), update_sp)
                    }

        case I_StorePair(src1, src2, dst, op2, update_sp) => op2 match {
                        case ImmVal(0) => addExclamation(printInstr3("stp", src1, src2, dst), update_sp)
                        case _ => addExclamation(printInstr4("stp", src1, src2, dst, op2), update_sp)
                    }
        
        case I_Move(dest, op) => printInstr2("mov", dest, op)
        case I_Movk(dest, op, shift) => printInstr3("movk", dest, op, shift)
        
        case I_Branch(label, condition) => condition match {
                        case null => s"      b ${label.labelName} \n"
                        case _ => s"      b.$condition ${label.labelName} \n"
                    }       
        case I_BranchLink(dstLabel) => s"      bl ${dstLabel.labelName} \n"
        
        case I_And(dest, src, op) => printInstr3("and", dest, src, op)
        case I_Xor(dest, src, op) => printInstr3("eor", dest, src, op)
        case I_Orr(dest, src, op) => printInstr3("orr", dest, src, op)
        case I_Neg(dest, src, shift) => printInstr3("neg", dest, src, shift)
        
        case I_Cmp(src, op) => printInstr2("cmp", src, op)
        case I_Cmp_Shift(src, op, shift) => printInstr3("cmp", src, op, shift)
        
        case I_Push(src) => printInstr1("push", src)
        case I_Pop(dest) => printInstr1("pop", dest)

        case I_Label(labelName) => s"$labelName: \n"
        case I_Directive(name) => s"$name \n"

        case I_ADRP(dest, label) => printInstr2withLabel("adrp", dest, label)
        case I_ADR(dest, label) => printInstr2withLabel("adr", dest, label)
        
        case I_Ret => s"      ret \n"
        
        case I_Cbz(reg, label) => printInstr2withLabel("cbz", reg, label)
        case I_CSet(dest, condition) => s"      cset ${dest.getValue()}, $condition \n"
        case I_Csel(reg1, reg2, reg3, condition) => s"      csel ${reg1.getValue()}, ${reg2.getValue()}, ${reg3.getValue()}, $condition \n"
        
        case I_Ldrsw(reg1, content) => printInstr2("ldrsw", reg1, content)
        case I_Tst(reg1, op) => printInstr2("tst", reg1, op)
        case I_Sbfx(reg1, reg2, start_bit, len) => printInstr4("sbfx", reg1, reg2, start_bit, len)
        case I_Sxtw(reg1, reg2) => printInstr2("sxtw", reg1, reg2)
        case _ => ""
    } 
  }
}