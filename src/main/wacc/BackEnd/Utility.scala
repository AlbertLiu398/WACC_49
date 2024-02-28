package wacc 

import wacc.ast._
import scala.collection._
import Instruction._
import scala.annotation.unused
import Constant._
import Conditions._
import Labels._
import ErrorMsgs._

object Utility {

    private val sT = new SymbolTable
    val codeGenerator = new CodeGenerator(sT.getVarList())

    var printCharFlag: Boolean = false
    var printIntFlag: Boolean = false
    var printStringFlag: Boolean = false
    var printBoolFlag: Boolean = false
    var printLineFlag: Boolean = false
    var mallocFlag: Boolean = false
    var readFlag: Boolean = false

    final val PRINT_STRING_LABEL = "_prints"
    final val PRINT_INT_LABEL = "_printi"
    final val PRINT_BOOL_LABEL = "_printb"
    final val PRINT_LN_LABEL = "_println"
    final val PRINT_CHAR_LABEL = "_printc"
    final val READ_LABEL = "_read"
    final val PRINT_F_LABEL = "printf"
    final val PRINT_P_LABEL = "_printp"
    final val PRINT_FLUSH_LABEL =  "fflush"

    final val MALLOC_LABEL = "_malloc"
    final val EXIT_LABEL = "_exit"
    final val SCANF_LABEL = "scanf"
    final val ERR_OUT_OF_MEMORY_LABEL = "_errOutOfMemory"
    final val ERR_NULL_LABEL = "_errNull"
    
    // val DIVIDE_BY_ZERO_LABEL = "_check_divide_by_zero"
    // val NULL_POINTER_LABEL = "_check_null_pointer"
    // val ARRAY_BOUNDS_LABEL = "_check_array_bounds"
    // val OVERFLOW_LABEL = "_throw_overflow_error"
    // val RUNTIME_LABEL = "_throw_runtime_error"


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
        if (readFlag) {
            read()
        }
        instrus
    }

    def printstr(): Unit = {

        addCustomisedDataMsg("%.*s", addPrintsLabel(false))

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label(PRINT_STRING_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x2, x0))
        instrus.append(I_LDRSW(x1, Content(x0, ImmVal(-4))))
        instrus.append(I_ADR(x0, I_Label(addPrintsLabel(true))))
        
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label( PRINT_FLUSH_LABEL)))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
        instrus.append(I_Ret)
    }

    def printint(): Unit = {

        addCustomisedDataMsg("%.*s", addPrintiLabel(false))

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label(PRINT_INT_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(addPrintiLabel(true))))

        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL))) 

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label( PRINT_FLUSH_LABEL)))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
    }

    def printbool(): Unit = {

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label(PRINT_BOOL_LABEL ))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Cmp(x0, ImmVal(0)))
        
        instrus.append(I_Branch(I_Label(".L._printb0"), NE))

        val labelPrint = addPrintbLabel(true)
        addCustomisedDataMsg("true", labelPrint)
        instrus.append(I_ADR(x2, I_Label(labelPrint)))

        instrus.append(I_Branch(I_Label(".L._printb1")))
        
        instrus.append(I_Label(".L._printb0"))
        
        val labelTrue = addPrintbLabel(true)
        addCustomisedDataMsg("false", labelTrue)
        instrus.append(I_ADR(x2, I_Label(labelTrue)))   
        
        instrus.append(I_Label(".L_.printb1"))    
        
        instrus.append(I_LDRSW(x1, Content(x2, ImmVal(-4))))
        
        val labelFalse = addPrintbLabel(true)
        addCustomisedDataMsg("%.*s", labelFalse)
        instrus.append(I_ADR(x0, I_Label(labelFalse)))
        
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        
        instrus.append(I_Move(x0, ImmVal(0)))
        
        instrus.append(I_BranchLink(I_Label( PRINT_FLUSH_LABEL)))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
        
    }

    def printchar(): Unit = {
        
        addCustomisedDataMsg("%.*s", addPrintcLabel(false))

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label(PRINT_CHAR_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(addPrintcLabel(true))))

        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label( PRINT_FLUSH_LABEL)))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)   
    }

    def printline(): Unit = {

        
        addCustomisedDataMsg("%.*s", addPrintlnLabel(false))

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label(PRINT_LN_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_ADR(x0, I_Label(addPrintlnLabel(true))))
        
        instrus.append(I_BranchLink(I_Label("puts")))

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label( PRINT_FLUSH_LABEL)))

        instrus.append (I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
    }


    def printp(): Unit = {
        
        addCustomisedDataMsg("%.*s", addPrintpLabel(false))

        instrus.append(I_Directive(".align 4"))

        instrus.append(I_Label(PRINT_P_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

        instrus.append(I_Move(x1, x0))

        instrus.append(I_ADR(x0, I_Label(addPrintpLabel(true))))

        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))

        instrus.append(I_Move(x0, ImmVal(0)))

        instrus.append(I_BranchLink(I_Label( PRINT_FLUSH_LABEL)))

        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))

        instrus.append(I_Ret)
    }



    def malloc(): Unit = {
        instrus.append(I_Label(MALLOC_LABEL))
        instrus.append(I_BranchLink(I_Label(MALLOC_LABEL)))
        instrus.append(I_CBZ(x0, I_Label(ERR_OUT_OF_MEMORY_LABEL)))
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
        instrus.append(I_Ret)
        errOutOfMemory()
    }

    private def errOutOfMemory(): Unit = {
        addCustomisedDataMsg(ERR_OUT_OF_MEMORY_MSG, ERR_OUT_OF_MEMORY_LABEL)
        instrus.append(I_Directive("align 4"))
        instrus.append(I_Label(ERR_OUT_OF_MEMORY_LABEL))
        instrus.append(I_BranchLink(I_Label(PRINT_STRING_LABEL)))
        instrus.append(I_BranchLink(I_Label(EXIT_LABEL)))
    }

    def read(): Unit = {
        addCustomisedDataMsg("%d", "_read_")
        instrus.append(I_Label(READ_LABEL))
        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x1, sp))
        instrus.append(I_ADR(x0, I_Label(addReadLabel())))
        instrus.append(I_BranchLink(I_Label(SCANF_LABEL)))
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
        instrus.append(I_Ret)
    }
    
}