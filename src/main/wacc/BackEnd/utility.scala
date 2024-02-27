package wacc 

import wacc.ast._
import wacc.instruction._
import scala.collection._
import instruction._
import scala.annotation.unused
import Constant._
import conditions._
import Labels._
import utility._

object utility {

    private val sT = new SymbolTable
    val codeGenerator = new CodeGenerator(sT.getVarList())

    var printCharFlag: Boolean = false
    var printIntFlag: Boolean = false
    var printStringFlag: Boolean = false
    var printBoolFlag: Boolean = false
    var printLineFlag: Boolean = false
    var mallocFlag: Boolean = false

    var instrus: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    def addUtility():  mutable.ListBuffer[Instruction]  = {
        if (printCharFlag) {
            printchar()
        }
        if (printIntFlag) {
            printint()
        }
        if (printStringFlag) {
            printstr()
        }
        if (printBoolFlag) {
            printbool()
        }
        if (printLineFlag) {
            printline()
        }
        if (mallocFlag) {
            malloc()
        }
        instrus
    }

    def printstr(): Unit = {
        addCustomisedDataMsg("%.*s", "_prints_")

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label("_prints"))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x2, x0))
        instrus.append(I_LDRSW(x1, Content(x0, ImmVal(-4))))
        instrus.append(I_ADR(x0, I_Label(".L._prints_str0")))

        codeGenerator.branchLink(instrus,"printf")
        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label("fflush")))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
        instrus.append(I_Ret)
    }

    def printint(): Unit = {
        addCustomisedDataMsg("%.*s", "_printi_")

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label("_println"))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(".L._printi_str0")))

        instrus.append(I_BranchLink(I_Label("printf"))) 

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label("fflush")))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
    }

    def printbool(): Unit = {
        addCustomisedDataMsg("%.*s", "_printb_")

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label("_println"))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(".L._printb_str0")))

        instrus.append(I_BranchLink(I_Label("printf")))


    }

    def printchar(): Unit = {
        addCustomisedDataMsg("%.*s", "_println_")

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label("_println"))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(".L._printc_str0")))

        instrus.append(I_BranchLink(I_Label("printf")))

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label("fflush")))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)   
    }

    def printline(): Unit = {
        addCustomisedDataMsg("%.*s", "_println_")

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label("_println"))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_ADR(x0, I_Label(".L._println_str0")))
        
        instrus.append(I_BranchLink(I_Label("puts")))

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label("fflush")))

        instrus.append (I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
    }


    def printpair(): Unit = {
        addCustomisedDataMsg("%.*s", "_printp_")

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label("_printp"))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(".L._printp_str0")))

        instrus.append(I_BranchLink(I_Label("printf")))

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label("fflush")))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
    }



    def malloc(): Unit = {
        instrus.append(I_Label("_malloc"))

        codeGenerator.branchLink(instrus, "_malloc")
        instrus.append(I_CBZ(x0, I_Label("_errOutOfMemory")))
        
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
        instrus.append(I_Ret)
        errOutOfMemory()
    }

    def errOutOfMemory(): Unit = {

        addCustomisedDataMsg("fatal error: out of memory\n", "_errOutOfMemory_")
        
        instrus.append(I_Directive("align 4"))

        instrus.append(I_Label("_errOutOfMemory"))

        codeGenerator.branchLink(instrus, "_prints")
        codeGenerator.branchLink(instrus, "_exit")
    }
}