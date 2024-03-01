package wacc 

import wacc.ast._
import scala.collection._
import Instruction._
import scala.annotation.unused
import Constant._
import Conditions._
import Labels._
import ErrorMsgs._
import Shift._

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
    var arrloadFlag: mutable.ListBuffer[Int] = mutable.ListBuffer()
    var arrstoreFlag: mutable.ListBuffer[Int] = mutable.ListBuffer()
    var errOutOfBoundFlag = false
    var freePairFlag = false

    // Labels for print functions
    final val PRINT_STRING_LABEL = "_prints"
    final val PRINT_INT_LABEL = "_printi"
    final val PRINT_BOOL_LABEL = "_printb"
    final val PRINT_LN_LABEL = "_println"
    final val PRINT_CHAR_LABEL = "_printc"
    final val PRINT_P_LABEL = "_printp"

    final val PRINT_F_LABEL = "printf"
    final val PUTS_LABEL = "puts"

    // Labels for read functions
    final val READI_LABEL = "_readi"
    final val READC_LABEL = "_readc"
    
    // Labels for other utility functions
    final val SCANF_LABEL = "scanf"
    final val FLUSH_LABEL =  "fflush"
    final val MALLOC_LABEL = "_malloc"
    final val MALLOC_FUNC_LABEL = "malloc"
    final val EXIT_LABEL = "_exit"
    final val ARRLOAD_LABEL = "_arrLoad"
    final val ARRSTORE_LABEL = "_arrStore"
    final val FREE_PAIR_LABEL = "_freePair"
    final val FREE_LABEL = "free"

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
        if (printBoolFlag) {
            printbool()
        }
        if (printLineFlag) {
            printline()
        }
        if (printPFlag) {
            printp()
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

         if (errOutOfBoundFlag) {
            printStringFlag = true
            errOutOfBounds()
        }

        if (badCharFlag) {
            printStringFlag = true
            errBadChar()
        }
        arrloadFlag = arrloadFlag.distinct
        if (!arrloadFlag.isEmpty) {
            for (size <- arrloadFlag) {
                arrLoad(size)
            }
        }

        arrstoreFlag = arrstoreFlag.distinct
        if (!arrstoreFlag.isEmpty) {
            for (size <- arrstoreFlag) {
                arrStore(size)
            }
        }

        if (freePairFlag) {
            freePair()
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
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        
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
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))

        printEnd()
    }

    def printbool(): Unit = {

        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_BOOL_LABEL ))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Cmp(x0, ImmVal(0)))
        
        instrus.append(I_Branch(I_Label(".L_printb0"), NE))

        
        
        val labelFalse = addPrintbLabel()
        addCustomisedDataMsg("pfalse", labelFalse)
        instrus.append(I_ADR(x2, I_Label(labelFalse)))  
        instrus.append(I_Branch(I_Label(".L_printb1"))) 
        
        instrus.append(I_Label(".L_printb0"))    
        val labelTrue = addPrintbLabel()
        addCustomisedDataMsg("ptrue", labelTrue)
        instrus.append(I_ADR(x2, I_Label(labelTrue)))

        instrus.append(I_Label(".L_printb1"))
        instrus.append(I_Ldrsw(x1, Content(x2, ImmVal(-4))))

        val labelString = addPrintbLabel()
        addCustomisedDataMsg("a%.*s", labelString)
        instrus.append(I_ADR(x0, I_Label(labelString)))
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        
        printEnd() 
    }

    def printchar(): Unit = {
        
        val label = addPrintcLabel()
        addCustomisedDataMsg("p%c", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_CHAR_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Move(x1, x0))
        instrus.append(I_ADR(x0, I_Label(label)))
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))

        printEnd()
    }

    def printline(): Unit = {

        val label = addPrintlnLabel()
        addCustomisedDataMsg("p", label)
        instrus.append(I_Directive(".align 4"))
        instrus.append(I_Label(PRINT_LN_LABEL))

        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_ADR(x0, I_Label(label)))
        instrus.append(I_BranchLink(I_Label(PUTS_LABEL)))
        
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
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))

        printEnd()
    }

    // helper function to extract common parts of print functions
    private def printEnd(): Unit = {
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
        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_BranchLink(I_Label(MALLOC_FUNC_LABEL)))
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
        instrus.append(I_ADR(x0, I_Label(label)))
        instrus.append(I_BranchLink(I_Label(PRINT_F_LABEL)))
        instrus.append(I_Move(x0, ImmVal(0)))
        instrus.append(I_BranchLink(I_Label(FLUSH_LABEL)))
        instrus.append(I_Move(x0, ImmVal(-1)))
        instrus.append(I_BranchLink(I_Label(EXIT_LABEL)))
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
    private def throwError(label: String): Unit = {
        instrus.append(I_ADR(x0, I_Label(label)))
        instrus.append(I_BranchLink(I_Label(PRINT_STRING_LABEL)))
        instrus.append(I_Move(x0, ImmVal(-1)))
        instrus.append(I_BranchLink(I_Label(EXIT_LABEL)))
    }


    def arrLoad(size: Int): Unit = {
        instrus.append(I_Label(ARRLOAD_LABEL + size))
        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Cmp(x17, ImmVal(0)))
        instrus.append(I_Csel(x1, x17, x1, LT))
        instrus.append(I_Branch(I_Label(ERR_OUT_OF_BOUND_LABEL), LT))
        instrus.append(I_Ldrsw(lr, Content(x7, ImmVal(-4))))
        instrus.append(I_Cmp(x17, lr))
        instrus.append(I_Csel(x1, x17, x1, GE))
        instrus.append(I_Branch(I_Label(ERR_OUT_OF_BOUND_LABEL), GE))
        if (size == 4) {
            instrus.append(I_Ldrsw(x7, Content(x7, x17, LSL(size/4 + 1))))
        } else {
            instrus.append(I_Load(x7, Content(x7, x17, LSL(size/4 + 1))))
        }
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
        instrus.append(I_Ret)
    }

    def arrStore(size: Int): Unit = {
        instrus.append(I_Label(ARRSTORE_LABEL + size))
        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Sxtw(x17, x17.toW()))
        instrus.append(I_Cmp(x17.toW(), ImmVal(0)))
        instrus.append(I_Csel(x1, x17, x1, LT))
        instrus.append(I_Branch(I_Label(ERR_OUT_OF_BOUND_LABEL), LT))
        instrus.append(I_Ldrsw(lr, Content(x7, ImmVal(-4))))
        instrus.append(I_Cmp(x17, lr))
        instrus.append(I_Csel(x1, x17, x1, GE))
        instrus.append(I_Branch(I_Label(ERR_OUT_OF_BOUND_LABEL), GE))
        instrus.append(I_Store(x8.toW(), Content(x7, x17, LSL(size/4 + 1))))
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
        instrus.append(I_Ret)
    }

    def freePair(): Unit = {
        instrus.append(I_Label(FREE_PAIR_LABEL))
        instrus.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instrus.append(I_Cbz(x0, I_Label(ERR_NULL_LABEL)))
        instrus.append(I_BranchLink(I_Label(FREE_LABEL)))
        instrus.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
        instrus.append(I_Ret)
    }
    
}