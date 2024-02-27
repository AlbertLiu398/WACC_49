package wacc 

import instruction._
import Labels._
import scala.collection._
import Constant._

object utility {

    var printCharFlag: Boolean = false
    var printIntFlag: Boolean = false
    var printStringFlag: Boolean = false
    var printBoolFlag: Boolean = false
    var printLineFlag: Boolean = false

    def prints(instruction: mutable.ListBuffer[Instruction]):Unit = {
        addCustomisedDataMsg("%.*s", "_prints_")

        instruction.append(I_Directive("align 4"))

        instruction.append(I_Label("_prints"))

        instruction.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instruction.append(I_Move(x2, x0))
        // instructions.append(I_LDRSW(x1, Content(x0, ImmVal(-4))))
        instruction.append(I_ADR(x0, I_Label(".L._prints_str0")))

        instruction.append(I_BranchLink(I_Label("printf")))
        instruction.append(I_Move(x0, ImmVal(0)))

        instruction.append(I_BranchLink(I_Label("fflush")))

        instruction.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
        instruction.append(I_Ret)
    }

    def printi(): Unit = {

    }

    def printb(): Unit = {

    }

    def printc(): Unit = {
        
    }

    // def branchLink(s: String): Unit = {
    //     instruction
    // }


}