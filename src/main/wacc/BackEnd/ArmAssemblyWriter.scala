package wacc
import Instruction._

object ArmAssemblyWriter extends AssemblyWriter {

  override def translateInstruction(instr: Instruction): String = {
    instr match {
        case I_Add(dest, src, op, update_flag) => update_flag match {
                        case false => s"      add ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
                        case true => s"      adds ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
                    }
        case I_Sub(dest, src, op, update_flag) => update_flag match {
                        case false => s"      sub ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
                        case true => s"      subs ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
                    }
        case I_ReverseSub(dest, src, op) => s"      subr ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        case I_Mul(dest, src, op) => s"      mul ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        case I_SMul(dest, src, op) => s"     smull ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        case I_SDiv(dest, src, op) => s"      sdiv ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        
        case I_Load(dest, op, op2, update_sp) =>  op2 match {
                        case ImmVal(0) => s"      ldr ${dest.getValue()}, ${op.getValue()}" + (if (update_sp) " !\n" else "\n")
                        case _ => s"      ldr ${dest.getValue()},, ${op.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                    }  
        case I_LoadSByte(dest, op, op2, update_sp) =>  op2 match {
                        case ImmVal(0) => s"      ldrb ${dest.getValue()}, ${op.getValue()}" + (if (update_sp) " !\n" else "\n")
                        case _ => s"      ldrp ${dest.getValue()},, ${op.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                    }
        case I_LoadPair(src1, src2, dst, op2, update_sp) => op2 match {
                        case ImmVal(0) => s"      ldp ${src1.getValue()}, ${src2.getValue()}, ${dst.getValue()}" + (if (update_sp) " !\n" else "\n")
                        case _ => s"      ldp ${src1.getValue()}, ${src2.getValue()}, ${dst.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                    }
        case I_Store(src, op, op2, update_sp) =>  op2 match {
                        case ImmVal(0) => s"      str ${src.getValue()}, ${op.getValue()}" + (if (update_sp) " !\n" else "\n")
                        case _ => s"      str ${src.getValue()},, ${op.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                    }
        case I_StoreByte(src, dest, op2, update_sp) =>  op2 match {
                        case ImmVal(0) => s"      strb ${src.getValue()}, ${dest.getValue()}" + (if (update_sp) " !\n" else "\n")
                        case _ => s"      strb ${src.getValue()},, ${dest.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                    }
        case I_StorePair(src1, src2, dst, op2, update_sp) => op2 match {
                        case ImmVal(0) => s"      stp ${src1.getValue()}, ${src2.getValue()}, ${dst.getValue()}" + (if (update_sp) " !\n" else "\n")
                        case _ => s"      stp ${src1.getValue()}, ${src2.getValue()}, ${dst.getValue()}, ${op2.getValue()}" + (if (update_sp) "!\n" else "\n")
                    }
        case I_Move(dest, op) => s"      mov ${dest.getValue()}, ${op.getValue()} \n"
        case I_Movk(dest, op, shift) => s"      movk ${dest.getValue()}, ${op.getValue()}, ${shift.getValue()} \n"
        case I_Branch(label, condition) => condition match {
                        case null => s"      b ${label.labelName} \n"
                        case _ => s"      b.$condition ${label.labelName} \n"
                    }       
        case I_BranchLink(dstLabel) => s"      bl ${dstLabel.labelName} \n"
        case I_And(dest, src, op) => s"      and ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        case I_Xor(dest, src, op) => s"      eor ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        case I_Orr(dest, src, op) => s"      orr ${dest.getValue()}, ${src.getValue()}, ${op.getValue()} \n"
        case I_Cmp(src, op) => s"      cmp ${src.getValue()}, ${op.getValue()} \n"
        case I_Cmp_Shift(src, op, shift) => s"      cmp ${src.getValue()}, ${op.getValue()}, ${shift.getValue()} \n"
        
        case I_Push(src) => s"      push {${src.getValue()}} \n"
        case I_Pop(dest) => s"      pop {${dest.getValue()}} \n"

        case I_Label(labelName) => s"$labelName: \n"
        case I_Directive(name) => s"$name \n"
        case I_CSet(dest, condition) => s"      cset ${dest.getValue()}, $condition \n"
        case I_Neg(dest, src, shift) => s"      neg ${dest.getValue()}, ${src.getValue()}, ${shift.getValue()} \n"
        case I_ADRP(dest, label) => s"      adrp ${dest.getValue()}, ${label.labelName} \n"
        case I_ADR(dest, label) => s"      adr ${dest.getValue()}, ${label.labelName} \n"
        case I_Ret => s"      ret \n"
        case I_Cbz(reg, label) => s"      cbz ${reg.getValue()}, ${label.labelName} \n"
        case I_Ldrsw(reg1, content) => s"      ldrsw ${reg1.getValue()}, ${content.getValue()} \n"
        case I_Csel(reg1, reg2, reg3, condition) => s"      csel ${reg1.getValue()}, ${reg2.getValue()}, ${reg3.getValue()}, $condition \n"
        case I_Tst(reg1, op) => s"      tst ${reg1.getValue()}, ${op.getValue()} \n"
        case I_Sbfx(reg1, reg2, start_bit, len) => s"      sbfx ${reg1.getValue()}, ${reg2.getValue()}, $start_bit, $len \n"
        case I_Sxtw(reg1, reg2) => s"      sxtw ${reg1.getValue()}, ${reg2.getValue()} \n"
        case _ => ""
    } 
  }
}