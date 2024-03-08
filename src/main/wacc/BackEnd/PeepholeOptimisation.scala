package wacc 

import scala.collection.mutable
import Instruction._

object PeepholeOptimisation {       

    var resultList: mutable.ListBuffer[Instruction] = mutable.ListBuffer()

    def runPeeopholeOptimisation(instructions: List[Instruction]): List[Instruction] = {
        
        resultList = mutable.ListBuffer(instructions: _*)
        runPeeopholeSize2(resultList)
        resultList.toList
    }

    def runPeeopholeSize2(instructions: mutable.ListBuffer[Instruction]): Unit = {
        var length = resultList.length
        var i = 0
        while (i <= length - 2) {
            var changed = false
            var stackChanged = false
            resultList(i) match {
                case I_Move(_, _) => {
                    changed = optimiseMove(i, i + 1)
                }
                case I_Load(_, _, _, _, _) => {
                    changed = optimiseLoad(i, i + 1)
                }
                case I_Store(_, _, _, _, _) => {
                    changed = optimiseStore(i, i + 1)
                }
                case I_LoadPair(_, _, _, _, _) => {
                    stackChanged = optimisePopPush(i, i + 1)
                }
                case I_StorePair(_, _, _, _, _) => {
                    stackChanged = optimisePushPop(i, i + 1)
                }
                case _ => {}
            }
            // if no change, move to next instruction

            if (!changed && !stackChanged) {
                i += 1
            }
            if (stackChanged && i > 0) {
                i -= 1
            }
            // update length
            length = resultList.length
        }
    }

// -------------peephole size 2---------------

    def optimiseMove(fstIndex: Int, sndIndex: Int): Boolean = {

    /*
        1. Redundant Move-Move (only through x8)
        e.g.
            mov     x8, x1
            mov     x2, x8
        can be replaced with
            mov     x2, x1
    
        2. Redundant Move-Store (only through x8, second operand of mov must be register)
        e.g.
            mov     x8, x1
            str     x8, [fp,#28]
        can be replaced with
            str     x1, [fp,#28]
    
        3. Redundant Move-Load (Not very common)
        e.g.
            mov     w0, w1
            ldr     w0, [fp,#28]
        can be replaced with
            ldr     w0, [fp,#28]
        (remove first instruction)
    */

        val fstInstr = resultList(fstIndex)
        fstInstr match {
            // src1 can be a register or immediate value
            case I_Move(dst1@Reg(n1, size1), src1) => {
                val sndInstr = resultList(sndIndex)
                sndInstr match {
                    // Move-Move
                    case I_Move(dst2, src2@Reg(n2, size2)) => {
                        // If two instructions are the same, remove the second one
                        if (dst1.equals(dst2) && src1.equals(src2)) {
                            // remove the second instruction 
                            resultList.remove(sndIndex)
                            return true
                        }
                        // If the second instruction is a move from x8, and the first instruction is a move to x8
                        if (n1 == 8 && n2 == 8 && size1 == size2) {
                            // remove the second instruction and rewrite the first
                            resultList.remove(sndIndex)
                            resultList(fstIndex) = I_Move(dst2, src1)
                            return true
                        }
                    }
                    // Move-Store
                    case I_Store(src2@Reg(n2, size2), dst2, op2, us, false) => {
                        // If the second instruction is a store from x8, and the first instruction is a move to x8
                        // the first operand of store must be a register
                        if (n1 == 8 & n2 == 8 && size1 == size2 && src1.isInstanceOf[Register]) {
                            // rewrite the second instruction and remove the first
                            resultList(sndIndex) = I_Store(src1.asInstanceOf[Register], dst2, op2, us, false)
                            resultList.remove(fstIndex)
                            return true
                        }    
                    }
                    // Move-Load
                    case I_Load(dst2, op, op2, us, false) => {
                        if (dst1.equals(dst2)) {
                            // remove the first instruction
                            resultList.remove(fstIndex)
                            return true
                        }
                    }
                    case _ => {}
                }
                
            }
            case _ => {}
        }
        return false
    }

    def optimiseLoad(fstIndex: Int, sndIndex: Int): Boolean = {
        /*
        1. Redundant Load-Load
        e.g.
            ldr     w1, [fp,#28]
            ldr     w0, [fp,#28]
        can be replaced with
            ldr     w1, [fp,#28]
            mov     w0, w1
        ------------------------
        e.g.
            ldr     w1, [fp,#28]
            ldr     w1, [fp,#32]
        can be replaced with
            ldr     w1, [fp,#32]
        (first instruction is overwritten by second instruction)


        2. Redundant Load-Move
        e.g.
            ldr     w0, [fp,#28]
            mov     w0, w1
        can be replaced with
            mov     w0, w1
        (first instruction is overwritten by second instruction)
    
        3. Redundant Load-Store
        e.g.
            ldr     w0, [fp,#28]
            str     w0, [fp,#28]
        can be replaced with
            ldr     w0, [fp,#28]
        (remove second instruction)
        */

        val fstInstr = resultList(fstIndex)
        fstInstr match {
            case I_Load(dst1, op_1, op2_1, us, false) => {
                val sndInstr = resultList(sndIndex)
                sndInstr match {
                    // Load-Load
                    case I_Load(dst2, op_2, op2_2, us, false) => {
                        // different dst, same op/op2
                        if (!dst1.equals(dst2) && op_1.equals(op_2) && op2_1.equals(op2_2)){
                            // rewrite the second instruction, the first one remains
                            resultList(sndIndex) = I_Move(dst2, dst1)
                            return true
                        }
                        // same dst, different op/op2
                        if (dst1.equals(dst2) && (!op_1.equals(op_2) || !op2_1.equals(op2_2))){
                            // remove the first instruction
                            resultList.remove(fstIndex)
                            return true
                        }
                    }

                    // Load-Move
                    case I_Move(dst2, src2) => {
                        if (dst1.equals(dst2)) {
                            // remove the second instruction
                            resultList.remove(fstIndex)
                            return true
                        }
                    }
                    
                    // Load-Store
                    case I_Store(src2, dst2, op2_2, us, false) => {
                        if (dst1.equals(src2) && op_1.equals(dst2) && op2_1.equals(op2_2)){
                            // remove the second instruction
                            resultList.remove(sndIndex)
                            return true
                        }
                        
                    }
                    case _ => {}
                }
            }
            case _ => {}

        }
        return false


    }

    def optimiseStore(fstIndex: Int, sndIndex: Int): Boolean = {
    /*
        Redundant Store-Store
        e.g.
            str     w0, [fp,#28]
            str     w1, [fp,#28]
        can be replaced with
            str     w1, [fp,#28]
        (remove first instruction)  

        Redundant Store-Load
        e.g.
            str     w0, [fp,#28]
            ldr     w1, [fp,#28]
        can be replaced with
            str     w0, [fp,#28]
            mov     w1, w0
        --------------------------
        e.g. 
            str     w0, [fp,#28]
            ldr     w0, [fp,#28]
        can be replaced with
            str     w0, [fp,#28]
        (remove second instruction)
    */

        val fstInstr = resultList(fstIndex)
        fstInstr match {
            case I_Store(src1, dst1, op2_1, us, false) => {
                val sndInstr = resultList(sndIndex)
                sndInstr match {
                    // Store-Store
                    case I_Store(src2, dst2, op2_2, us, false) => {
                        if (dst1.equals(dst2) && op2_1.equals(op2_2)) {
                            // remove the first instruction
                            resultList.remove(fstIndex)
                            return true
                        }
                    }
                    // Store-Load
                    case I_Load(dst2, op_2, op2_2, us, false) => {
                        // different dst, same op/op2
                        if (!src1.equals(dst2) && dst1.equals(op_2) && op2_1.equals(op2_2)) {
                            // rewrite the second instruction, the first one remains
                            resultList(sndIndex) = I_Move(dst2, src1)
                            return true
                        }
                        // same dst, same op/op2
                        if (src1.equals(dst2) && op_2.equals(dst1) && op2_1.equals(op2_2)) {
                            // remove second instruction
                            resultList.remove(sndIndex)
                            return true
                        }
                    }
                    case _ => {}
                }
            }
            case _ => {}
        }
        return false
    }



    // ---------STACK------------

    def optimisePopPush(fstIndex: Int, sndIndex: Int): Boolean = {
        /* 
        POP x1
        POP x2
        PUSH x2
        PUSH x1
        */

        val fstInstr = resultList(fstIndex)
        fstInstr match {
            case I_LoadPair(dst1, dst2, Content(reg1, ImmVal(0), _), ImmVal(n1), false) => {
                val sndInstr = resultList(sndIndex)
                sndInstr match {
                    case I_StorePair(src1, src2, Content(reg2, ImmVal(n2), _), ImmVal(0), true) => {
                        if (src1.equals(dst1) && src2.equals(dst2) && reg1.equals(reg2) && (n1 + n2 == 0)) {
                            // remove both instructions
                            resultList.remove(sndIndex)
                            resultList.remove(fstIndex)
                            return true
                        }
                    }
                    case _ => {}
                }
            }
            case _ => {}
        }
        return false
    }

    def optimisePushPop(fstIndex: Int, sndIndex: Int): Boolean = {
        /* 
        PUSH x1
        PUSH x2
        POP x2
        POP x1
        */
        val fstInstr = resultList(fstIndex)
        fstInstr match {
            case I_StorePair(src1, src2, Content(reg1, ImmVal(n1), _), ImmVal(0), true) => {
                val sndInstr = resultList(sndIndex)
                sndInstr match {
                    case I_LoadPair(dst1, dst2, Content(reg2, ImmVal(0), _), ImmVal(n2), false) => {
                        if (src1.equals(dst1) && src2.equals(dst2) && reg1.equals(reg2) && (n1 + n2 == 0)) {
                            // remove both instructions
                            resultList.remove(sndIndex)
                            resultList.remove(fstIndex)
                            return true
                        }
                    }
                    case _ => {}
                }
            }
            case _ => {}
        }
        return false
    }
    

    

    // ----------ARITHMETIC------------





// -------------peephole size 3---------------

    


    


}