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

    // Flags for print functions
    var printCharFlag: Boolean = false
    var printIntFlag: Boolean = false
    var printStringFlag: Boolean = false
    var printBoolFlag: Boolean = false
    var printLineFlag: Boolean = false
    var printPFlag: Boolean = false


    // Flags for read functions
    var readIntFlag: Boolean = false
    var readCharFlag: Boolean = false

    // Flags for other utility checks
    var mallocFlag: Boolean = false
    var divByZeroFlag: Boolean = false
    var arrayBoundsFlag: Boolean = false
    var arithmeticFlag: Boolean = false
    var nullPointerFlag: Boolean = false
    var badCharFlag: Boolean = false

    // Labels for print functions
    final val PRINT_STRING_LABEL = "_prints"
    final val PRINT_INT_LABEL = "_printi"
    final val PRINT_BOOL_LABEL = "_printb"
    final val PRINT_LN_LABEL = "_println"
    final val PRINT_CHAR_LABEL = "_printc"
    final val PRINT_F_LABEL = "printf"
    final val PRINT_P_LABEL = "_printp"

    // Labels for read functions
    final val READI_LABEL = "_readi"
    final val READC_LABEL = "_readc"
    
    // Labels for other utility functions
    final val SCANF_LABEL = "scanf"
    final val FLUSH_LABEL =  "fflush"
    final val MALLOC_LABEL = "_malloc"
    final val EXIT_LABEL = "_exit"

    // Labels for error messages 
    final val ERR_OUT_OF_MEMORY_LABEL = "_errOutOfMemory"
    final val ERR_OUT_OF_BOUND_LABEL = "_errOutOfBounds"
    final val ERR_NULL_LABEL = "_errNull"
    final val ERR_OVERFLOW_LABEL = "_errOverflow"
    final val ERR_DIV_ZERO_LABEL = "_errDivZero"
    final val ERR_BAD_CHAR_LABEL = "_errBadChar"


    
    // val OVERFLOW_LABEL = "_throw_overflow_error"
    // val RUNTIME_LABEL = "_throw_runtime_error"

    // Local list of instructions, to be appended to final instructions list in CodeGenerator
    var instrus: mutable.ListBuffer[Instruction] = mutable.ListBuffer.empty

    def addUtility():  mutable.ListBuffer[Instruction]  = {

        // ----print-----

        if (printCharFlag) {
            printchar()
        }
        if (printIntFlag) {
            printint()
        }
        // if (printStringFlag) {
        //     printstr()
        // }
        if (printBoolFlag) {
            printbool()
        }
        if (printLineFlag) {
            printline()
        }

        // ----read----

        if (readCharFlag) {
            readchar()
        }
        if (readIntFlag) {
            readint()
        }


        // ----other utilities----

        if (mallocFlag) {
            printStringFlag = true
            malloc()
        }

        if (divByZeroFlag) {
            printStringFlag = true
            errDivByzero()
        }

        if (nullPointerFlag) {
            printStringFlag = true
            errNull()
        }

        if (arrayBoundsFlag) {
            printStringFlag = true
            errOutOfBounds()
        }

        if (arithmeticFlag) {
            printStringFlag = true
            errOverFlow()
        
        }

        if (badCharFlag) {
            printStringFlag = true
            errBadChar()
        }

        // Finally check if need to add prints
        if (printStringFlag) {
            printstr()
        }
        
        instrus
    }

    // -----------------print functions-------------------- 

    def printstr(): Unit = {
        val label = addPrintsLabel()
        addCustomisedDataMsg("p%.*s", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_STRING_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x2, x0))
        instrus.append(I_Ldrsw(x1, Content(x0, ImmVal(-4))))
        instrus.append(I_ADR(x0, I_Label(label)))
        
        printEnd()
    }

    def printint(): Unit = {
        val label = addPrintiLabel()
        addCustomisedDataMsg("p%d", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_INT_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x1, x0))
        instrus.append(I_ADR(x0, I_Label(label)))

        printEnd()
    }

    def printbool(): Unit = {

        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_BOOL_LABEL ))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Cmp(x0, ImmVal(0)))
        
        instrus.append(I_Branch(I_Label(".L_printb0"), NE))

        val labelPrint = addPrintbLabel()
        addCustomisedDataMsg("ptrue", labelPrint)
        instrus.append(I_ADR(x2, I_Label(labelPrint)))

        instrus.append(I_Branch(I_Label(".L_printb1")))
        
        instrus.append(I_Label(".L_printb0"))
        
        val labelTrue = addPrintbLabel()
        addCustomisedDataMsg("pfalse", labelTrue)
        instrus.append(I_ADR(x2, I_Label(labelTrue)))   
        
        instrus.append(I_Label(".L_printb1"))    
        
        instrus.append(I_Ldrsw(x1, Content(x2, ImmVal(-4))))
        
        val labelFalse = addPrintbLabel()
        addCustomisedDataMsg("p%.*s", labelFalse)
        instrus.append(I_ADR(x0, I_Label(labelFalse)))
        
        printEnd()
        
    }

    def printchar(): Unit = {
        
        val label = addPrintcLabel()
        addCustomisedDataMsg("p%.*s", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_CHAR_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x1, x0))
        instrus.append(I_ADR(x0, I_Label(label)))

        printEnd()
    }

    def printline(): Unit = {

        val label = addPrintlnLabel()
        addCustomisedDataMsg("p", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_LN_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_ADR(x0, I_Label(label)))
        
        printEnd()
    }


    def printp(): Unit = {

        val label = addPrintpLabel()
        addCustomisedDataMsg("p%p", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_P_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x1, x0))
        instrus.append(I_ADR(x0, I_Label(label)))

        printEnd()
    }

    // helper function to extract common parts of print functions
    private def printEnd(): Unit = {
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        instrus.append(I_Move(x0, ImmVal(0)))
        instrus.append(I_BranchLink(I_Label( FLUSH_LABEL)))
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
        instrus.append(I_Ret)
    }



    // -----------------read functions-------------------- 

    def readint(): Unit = {
        val labelRead = addReadiLabel()
        addCustomisedDataMsg("r%d", labelRead)
        instrus.append(I_Label(READI_LABEL))

        read(labelRead)
    }

    def readchar() : Unit = {
        val labelRead = addReadcLabel()
        addCustomisedDataMsg("r %c", labelRead)
        instrus.append(I_Label(READC_LABEL))
        
        read(labelRead)

    }

    // helper function to extract shared instructions of readint and readchar
    private def read(labelRead : String) : Unit = {
        instrus.append(I_StorePair(x0, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x1, sp))
        instrus.append(I_ADR(x0, I_Label(labelRead)))
        instrus.append(I_BranchLink(I_Label(SCANF_LABEL)))
        instrus.append(I_LoadPair(x0, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
        instrus.append(I_Ret)
    }

    //  ------------ other utility functions (malloc) -----------------

    def malloc(): Unit = {
        instrus.append(I_Label(MALLOC_LABEL))
        instrus.append(I_BranchLink(I_Label(MALLOC_LABEL)))
        instrus.append(I_Cbz(x0, I_Label(ERR_OUT_OF_MEMORY_LABEL)))
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
        instrus.append(I_Ret)
        errOutOfMemory()
    }


    // --------------error handlers----------------------------

    private def errOutOfMemory(): Unit = {
        val label = addErrOutOfMemoryLabel()
        addCustomisedDataMsg("e" + ERR_OUT_OF_MEMORY_MSG, label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(ERR_OUT_OF_MEMORY_LABEL))
        throwError(label)
    }

    def errDivByzero(): Unit = {
        val label = addErrDivZeroLabel()
        addCustomisedDataMsg("d" + ERR_DIV_ZERO_MSG, label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(ERR_DIV_ZERO_LABEL))
        throwError(label)
    }

    def errNull() : Unit  = {
        val label = addErrNullLabel()
        addCustomisedDataMsg("e" + ERR_NULL_MSG, label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(ERR_NULL_LABEL))
        throwError(label)
    }

    def errOutOfBounds(): Unit = {
        val label = addErrOutOfBoundLabel()
        addCustomisedDataMsg("e" + ERR_OUT_OF_BOUND_MSG, label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(ERR_OUT_OF_BOUND_LABEL))
        throwError(label)
    }

    def errOverFlow(): Unit = {
        val label = addErrOverflowLabel()
        addCustomisedDataMsg("e" + ERR_OVERFLOW_MSG, label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(ERR_OVERFLOW_LABEL))
        throwError(label)
    }

    def errBadChar(): Unit = {
        val label = addErrBadCharLabel()
        addCustomisedDataMsg("e" + ERR_BAD_CHAR_MSG, label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(ERR_BAD_CHAR_LABEL))
        instrus.append(I_ADR(x0, I_Label(label)))
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        instrus.append(I_Move(x0, ImmVal(0)))
        instrus.append(I_BranchLink(I_Label(FLUSH_LABEL)))
        instrus.append(I_Move(x0, ImmVal(-1)))
        instrus.append(I_BranchLink(I_Label(EXIT_LABEL)))
    }

    
   // helper function to extract common parts of error handlers
    def throwError(label: String): Unit = {
        
        instrus.append(I_ADR(x0, I_Label(label)))
        instrus.append(I_BranchLink(I_Label(PRINT_STRING_LABEL)))
        instrus.append(I_Move(x0, ImmVal(-1)))
        instrus.append(I_BranchLink(I_Label(EXIT_LABEL)))
    }
    
}