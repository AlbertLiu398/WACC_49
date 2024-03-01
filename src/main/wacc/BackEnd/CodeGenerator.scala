package wacc

import wacc.ast._
import scala.collection._
import Instruction._
import scala.annotation.unused
import Constant._
import Conditions._
import Labels._
import Utility._
import Shift._

class CodeGenerator (varList: List[Int]) {

  // identMap to store all variables and their size & pointer
  // For functions specifically: size = func.returnType's size
  case class identMapEntry(size: Int, reg: Register)

  private val identMap = mutable.Map[Ident, identMapEntry]()
  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()

  private var funcProcessed = 0

  private var assemblyFcuntions = mutable.Map[String, List[Instruction]]()

  private var inFunc = false
  private val funcIdentMap = mutable.Map[Ident, identMapEntry]()
  private val funcMap = mutable.Map[Ident, ParamList]()
  
  private var funcReturned = false
  
  final val MOV_MAX: Int = 65536
  final val MOV_MIN: Int = -65537
  /* 
    generateInstructions (recursive function)
  */
  def generateInstructions(ast: ASTNode): Unit = ast match {
    /* --------------------------- generate program */
    case Program(functions, statements) =>
      

      // Start with the data section
      instructions ++= List(
        I_Directive(".align 4"),
        I_Directive(".text"),
        I_Directive(".global main"), 
        I_Label("main"))

      // Put Functions into identMap
      for(func <- functions) {
        funcMap(func.functionName) = func.params
        identMap(func.functionName) = identMapEntry(getSize(func.returnType), xzr)
      }
      
      // Generate code for main body
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
      pushUsedRegs(unused_ResultRegs.toList, varList.last)
      instructions.append(I_Move(fp, sp))
      generateInstructions(statements)
      instructions.append(I_Move(x0, ImmVal(0)))
      popUsedRegs(used_ResultRegs.toList.reverse, varList.last)
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16)))
      instructions.append(I_Ret)
      revertResultRegs()

      // Generate code for functions
      inFunc = true
      for(func <- functions) {
        funcReturned = false
        generateInstructions(func)
      }
      inFunc = false

      // Add utility function definitions
      instructions.appendAll(addUtility())
      
      // Add all string labels at the top of program
      if (allDataMsgs.nonEmpty) {

        // Add the DataMsg.instruction of all (String, DataMsg) pairs in the hashmap before all other instructions
        allDataMsgs.values.flatten.foreach(dataMsg => instructions.prependAll(dataMsg.instruction))

        // Add .data directive to the very top of program
        I_Directive(".data") +=: instructions
      }

      


    case Func(returnType, functionName, params, body) =>

      instructions.append(I_Label("wacc_"+functionName.value))
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
      pushUsedRegs(unused_GeneralRegs_copy, varList(funcProcessed))
      instructions.append(I_Move(fp, sp))
      generateInstructions(body)
      popUsedRegs(used_ResultRegs.toList.reverse, varList(funcProcessed))
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16)))
      instructions.append(I_Ret)
      funcProcessed += 1

    /* --------------------------- generate expressions */
    case Add(expr1, expr2) =>
      arithmeticFlag = true

      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append((I_Adds(x8.toW(), x8.toW(), ImmVal(value))))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          val fstReg = used_TempRegs.head
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Adds(x8.toW(), fstReg.toW(), x8.toW()))

      }
      checkOverflowHandler() 

      
    case Sub(expr1, expr2) => 
      arithmeticFlag = true

      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Subs(x8.toW(), x8.toW(), ImmVal(value)))
        
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          val fstReg = used_TempRegs.head
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Subs(x8.toW(), fstReg.toW(), x8.toW()))
      }
      checkOverflowHandler()

      

    case Mul(expr1, expr2) =>
      arithmeticFlag = true
      
      generateInstructions(expr1)  // mov x8 expr1
      instructions.append(I_Move(unused_TempRegs.head, x8))  // mov x9 x8 
      
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)

      generateInstructions(expr2)  //mov x8 epr2 
      
      instructions.append(I_SMul(x8, fstReg.toW(), x8.toW()))  // x8 = w9 * w8
      // take the 31st bit, sign extend it to 64 bits
      instructions.append(I_Sbfx(fstReg, x8, ImmVal(31), ImmVal(1)))
      // now take the top 32 bits of the result, shift and sign extend to 64 bits
      instructions.append(I_Cmp_Shift(fstReg, x8, ASR(32)))
      // if they are not equal then overflow occured
      instructions.append(I_Branch(I_Label(ERR_OVERFLOW_LABEL), NE))

    case Div(expr1, expr2) =>
      arithmeticFlag = true
      divByZeroFlag = true

      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          // Store the quotient
          val fstReg = unused_TempRegs.head
          instructions.append(I_Move(fstReg, ImmVal(value)))
          // Check for division by zero
          instructions.append(I_Cbz(fstReg, I_Label(ERR_DIV_ZERO_LABEL)))
          // Perform division
          instructions.append(I_SDiv(x8, x8, fstReg))
          
        case _=> 
          // x8 contains divdent, Move x8 to another register 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          // add the new register to used_TempRegs
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
        
          // fstReg is the new register, containing the divident
          val fstReg = used_TempRegs.head
          // remove the new register from unused_TempRegs
          unused_TempRegs.remove(0)

          // Store the quotient in x8
          generateInstructions(expr2)

          // Need to compare x8 with 0 to check for division by zero
          instructions.append(I_Cbz(x8, I_Label(ERR_DIV_ZERO_LABEL))) 
          instructions.append(I_SDiv(x8, fstReg, x8))
      }
      checkOverflowHandler()

    case Mod(expr1, expr2) =>
      arithmeticFlag = true
      divByZeroFlag = true
       
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cbz(fstReg, I_Label(ERR_DIV_ZERO_LABEL)))
      instructions.append(I_SDiv(unused_TempRegs.head, fstReg, x8))
      instructions.append(I_Mul(unused_TempRegs.head, unused_TempRegs.head, x8))
      instructions.append(I_Sub(x8, fstReg, unused_TempRegs.head))
      checkOverflowHandler()
      


    case LessThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(fstReg, x8))
      instructions.append(I_CSet(x8, LT))
      //push and pop x8
        
    case LessThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(fstReg, x8))
      instructions.append(I_CSet(x8, LE))

    case GreaterThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(fstReg, x8))
      instructions.append(I_CSet(x8, GT))

    case GreaterThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(fstReg, x8))
      instructions.append(I_CSet(x8, GE))

    case Eq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(fstReg, x8))
      instructions.append(I_CSet(x8, EQ))
    
    case NotEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      val fstReg = used_TempRegs.head
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(fstReg, x8))
      instructions.append(I_CSet(x8, NE))

    case And(expr1, expr2) =>
      // bool x = expr1 && (1 && 0)
      // x8 : expr1 
      // x8 
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_And(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          val fstReg = used_TempRegs.head
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_And(x8, fstReg, x8))
      }

    case Or(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Orr(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          val fstReg = used_TempRegs.head
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Orr(x8, fstReg, x8))
      }

    case Invert(expr) =>
      generateInstructions(expr)
      instructions.append(I_Cmp(x8, ImmVal(1)))
      instructions.append(I_CSet(x8, NE))
      //push and pop x8
      pushAndPopx8(16)
      


    case Negate(expr) =>
      // Need to check for overflow if the max negative number is negated to be positive, 
      // which will exceed the upper bound causing overflow
      arithmeticFlag = true
      
      expr match {
        case IntLiter(value) => instructions.append(I_Move(x8, ImmVal(-value)))
        case _=>
          generateInstructions(expr)
          instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
          instructions.append(I_LoadPair(x9, xzr, Content(sp), ImmVal(16)))
          instructions.append(I_Move(x8, ImmVal(0)))
          instructions.append(I_Subs(x8.toW(), x8.toW(), x9.toW()))
          checkOverflowHandler()
      }

    case Ord(expr) =>
      generateInstructions(expr)


    case Chr(expr) =>   
      badCharFlag = true
      generateInstructions(expr)

      //check the number of char is in ASCII table
      instructions.append(I_Tst(x8, ImmVal(0xffffff80)))
      instructions.append(I_Csel(x8, x8, x8, NE))
      instructions.append(I_Branch(I_Label(ERR_BAD_CHAR_LABEL), NE))

      pushAndPopx8(16)

   // -------------------------- Generate instructions for statements
    case Read(lValue) =>

        generateInstructions(lValue)
    
        val n = getIdent(lValue)
        // instructions.append(I_Cbz(x19, I_Label(ERR_NULL_LABEL)))
        // instructions.append(I_Load(x8, Content(x19, ImmVal(0))))
      
        
        // instructions.append(I_Move(x8, getRegFromMap(n)))
        instructions.append(I_Move(x0, x8))

        val readBranch = lValue.getType match {
          case "int" => 
            readIntFlag = true
            branchLink(READI_LABEL)
          case "char" =>
            readCharFlag = true
            branchLink(READC_LABEL)
        }
        instructions.append(I_Move(x16, x0))
        instructions.append(I_Move(x8, x16))
        instructions.append(I_Move(getRegFromMap(n), x8))

      
        
    case NewAssignment(identType, name, value) =>

      // nullPointerFlag = true
       
      generateInstructions(value)
      instructions.append(I_Move(unused_ResultRegs.head, x8))
      used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
      unused_ResultRegs.remove(0)
      identMap(name) = identMapEntry(getSize(value), used_ResultRegs.head)
      revertTempRegs()
          
      

    case Assignment(name, value) => 
      
      val reg = getRegFromMap(getIdent(name))
      name match {
        case n@ArrElem(name, v)=>
          var counter = v.length * 4
          for (expr <- v) {
            arrloadFlag += counter
            generateInstructions(expr)
            if (counter != 4) {
              instructions.append(I_Move(x17, x8))
              if (counter !=  v.length * 4) {
                instructions.append(I_LoadPair(x9, xzr, Content(sp), ImmVal(16)))
              }
              instructions.append(I_Move(x7, getRegFromMap(name)))
              branchLink(s"_arrLoad$counter")
              counter -= 4
            
              instructions.append(I_Move(x8, x7))
              instructions.append(I_Move(x8, x8))
              instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
            }
            else {
              var fstReg = getRegFromMap(name)
              if (v.length != 1) {
                instructions.append(I_LoadPair(unused_TempRegs.head, xzr, Content(sp), ImmVal(16)))
                used_TempRegs = unused_TempRegs.head +: used_TempRegs
                var fstReg = used_TempRegs.head
                unused_TempRegs.remove(0)
              }
              instructions.append(I_Move(x17, x8))
              generateInstructions(value)
              instructions.append(I_Move(x7, fstReg))
              
              branchLink(s"_arrLoad$counter")
            }
          errOutOfBoundFlag = true
        }

        case n@FstPairElem(values) =>
          nullPointerFlag = true
          instructions.append(I_Cbz(reg, I_Label(ERR_NULL_LABEL)))
        
        case n@SndPairElem(values) => 
          nullPointerFlag = true
          instructions.append(I_Cbz(reg, I_Label(ERR_NULL_LABEL)))

        case _=>
          generateInstructions(name)
          generateInstructions(value)
          
         
        instructions.append(I_Move(reg, x8))
        revertTempRegs()
        
      }
      
    
    case Free(expr) => 
      generateInstructions(expr)
      instructions.append(I_Move(x0, x8))
      val t = expr.getType
      print (t)
      if (t.contains("[]")) {

        branchLink(FREE_LABEL)
      } else {
        branchLink(FREE_PAIR_LABEL)
        freePairFlag = true
      }
      nullPointerFlag = true

    case Return(expr) => 

      if (!funcReturned){
        generateInstructions(expr)
        instructions.append(I_Move(x0, x8))
        funcReturned = true
      }
      
      
    case Exit(expr) =>
      generateInstructions(expr)
      instructions.append(I_Move(unused_ParamRegs.head, x8))
      branchLink(EXIT_LABEL)

    

// 13		adrp x8, .L.str0
// 14		add x8, x8, :lo12:.L.str0
// 15		// push {x8}
// 16		stp x8, xzr, [sp, #-16]!
// 17		// pop {x8}
// 18		ldp x8, xzr, [sp], #16
// 19		mov x8, x8
// 20		mov x0, x8
// 21		// statement primitives do not return results (but will clobber r0/rax)
// 22		bl _prints
// 23		mov x0, #0
// 24		// pop {fp, lr}
// 25		ldp fp, lr, [sp], #16
// 26		ret
    case Print(expr, newline) =>
      generateInstructions(expr)
      instructions.append(I_Move(x0, x8))

      val printBranch = expr.getType match {
        case "string" | "char[]" =>
          printStringFlag = true
          branchLink(PRINT_STRING_LABEL)

        case "bool" =>
          printBoolFlag = true
          branchLink(PRINT_BOOL_LABEL)

        case "char" =>
          printCharFlag = true
          branchLink(PRINT_CHAR_LABEL)

        case "int" =>
          printIntFlag = true
          branchLink(PRINT_INT_LABEL)

        case _ =>
          printPFlag = true
          branchLink(PRINT_P_LABEL)
      }
  
     
      if (newline) {
        printLineFlag = true
        branchLink( PRINT_LN_LABEL)
      }
      

    
    case If(condition, thenStat, elseStat) =>
      generateInstructions(condition)
      val (if_then, if_end) = addIfLabel()
      // Branch instruction, Jump to then clause if condition is EQ, otherwise continue to else clause
      condition match {
        case NotEq(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), NE))
        case LessThan(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), LT))
        case LessThanEq(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), LE))
        case GreaterThan(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), GT))
        case GreaterThanEq(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), GE))
        case Eq(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), EQ))
        case And(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), NE))
        case Or(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), NE))
        case _=> 
          instructions.append(I_Branch(I_Label(if_then), EQ)) 
      }
       

      // Generate else clause
      generateInstructions(elseStat)
      // Branch instruction, (Unconditional) Jump to end after executing else clause
      instructions.append(I_Branch(I_Label(if_end)))

      // Label for then clause
      instructions.append(I_Label(if_then))
      // Generate then clause
      generateInstructions(thenStat)

      // Label for end of if
      instructions.append(I_Label(if_end))

   
    case While(condition, stat) =>
      // Generate a pair of label strings
      val (while_condition, while_body) = addWhileLabel()

      // Unconditional Jump to while condition
      instructions.append(I_Branch(I_Label(while_condition)))

      // Label for start of body of while
      instructions.append(I_Label(while_body))
      // Generate body of while
      generateInstructions(stat)

      // Label for condition of while
      instructions.append(I_Label(while_condition))
      // Generate condition of while
      generateInstructions(condition)
      
      // Jump back to body of while if condition holds
      instructions.append(I_Branch(I_Label(while_body), NE))
      
    case Begin(stmt) =>
      generateInstructions(stmt)
    
    case SeqStmt(first, second) => 
      generateInstructions(first)
      generateInstructions(second)
        
    case Skip =>
    
    case NewPairRValue(expr1, expr2) =>

      mallocFlag = true

      instructions.append(I_Move(x0, ImmVal(getSize(expr1) + getSize(expr2))))

      branchLink(MALLOC_LABEL)

      instructions.append(I_Move(x16, x0))

      generateInstructions(expr1)

      instructions.append(I_Store(x8, Content(x16, ImmVal(0))))
      
      generateInstructions(expr2)

      instructions.append(I_Store(x8, Content(x16, ImmVal(getSize(expr1)))))

      instructions.append(I_Move(x8, x16))

      
    

    case IntLiter(value)=> 
      print("HERE")
      if (value > MOV_MAX || value < MOV_MIN) {

        instructions.append(I_Movz(x8, ImmVal(value & 0xFFFF), LSL(0)))
        instructions.append(I_Movk(x8, ImmVal(value >> 16), LSL(16)))
      } else {
        instructions.append(I_Move(x8, ImmVal(value)))
      }
         
    case BoolLiter(value) => 
      value match {
        case true => instructions.append(I_Move(x8, ImmVal(1)))
        case false => instructions.append(I_Move(x8, ImmVal(0)))
      }

    case CharLiter(value) => 
      instructions.append(I_Move(x8, ImmValChar(value)))

    case StringLiter(value) => 
      // Create label string with the given string 
      val label = Labels.addDataMsg(value)
      // Convert the label string to an I_Label class and store at x8
      instructions.append(I_ADRP(x8, I_Label(label)))
      instructions.append(I_Add(x8, x8, Op(s":lo12:$label")))
      pushAndPopx8(16)
      
    case ArrElem(name, value) => 
      var counter = value.length * 4
      for (expr <- value) {
        arrloadFlag += counter
        generateInstructions(expr)
        instructions.append(I_Move(x17, x8))
        instructions.append(I_Move(x7, getRegFromMap(name))) 
        branchLink(s"_arrLoad$counter")

        counter -= 4
        if (counter != 0) {
          instructions.append(I_Move(x8, x7))
          instructions.append(I_Move(x8, x8))
          instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
          instructions.append(I_Move(x7, ImmVal(1)))
          instructions.append(I_LoadPair(x9, xzr, Content(sp), ImmVal(16)))
        }
        errOutOfBoundFlag = true
      }

      
       // instructions.append(I_Move(x8, x8))

    
    
    case PairLiter => 
      instructions.append(I_Move(x8, ImmVal(0)))
      
    case a@ArrLiter(e, es) =>
      instructions.append(I_Move(x0, ImmVal(getSize(a))))
      branchLink(MALLOC_LABEL)
      instructions.append(I_Move(x16, x0))
      instructions.append(I_Add(x16, x16, ImmVal(ARRAY_ELEM_SIZE)))

      // Size of array  
      var arrSize = 0
      e match {
        case StringLiter("empty") => arrSize = 0
        case _=> arrSize = es.length + 1
      }
      instructions.append(I_Move(x8, ImmVal(arrSize)))

      instructions.append(I_Store(x8, Content(x16, ImmVal(-ARRAY_ELEM_SIZE))))
      var arrPointer = 0
      if (arrSize != 0) {
    for (expr <- e::es) {
          generateInstructions(expr)
          // StoreByte for char and bool
          instructions.append(I_Store(x8, Content(x16, ImmVal(arrPointer))))
          arrPointer += getSize(expr)
        }
      }
      

      instructions.append(I_Move(x8, x16))

      mallocFlag = true
    
    case CallRValue(func, args) =>
      for (i <- 0 until args.exprl.length) {
        generateInstructions(args.exprl(i))
        instructions.append(I_Move(unused_ParamRegs(i), x8))
        val p = getFuncFromMap(func).paramListType(i)
        funcIdentMap(p.paramName) = identMapEntry(getSize(p.paramType), unused_ParamRegs(i))
      }
      branchLink( "wacc_" + func.value)
      instructions.append(I_Move(x16, x0))
      instructions.append(I_Move(x8, x16))

    
    case FstPairElem(values) =>
      val reg = getRegFromMap(getIdent(values))
      instructions.append(I_Cbz(reg, I_Label("_errNull")))
      instructions.append(I_Load(x8, Content(reg, ImmVal(0))))

      nullPointerFlag = true
      

    case SndPairElem(values) => 
      val reg = getRegFromMap(getIdent(values))
      instructions.append(I_Cbz(reg, I_Label("_errNull")))
      instructions.append(I_Load(x8, Content(reg, ImmVal(8))))

      nullPointerFlag = true

    case Ident(value) => 


      if (inFunc) {
        var reg: Register = getFuncRegFromMap(Ident(value))
        if (reg == xzr) {
          reg = getRegFromMap(Ident(value))
        }
        instructions.append(I_Move(x8, reg))

      } else {
        instructions.append(I_Move(x8, getRegFromMap(Ident(value))))
      }
      

    case _ => 
      
  }


  /* ------------------- Helper functions ------------------- */
  def getInstructions(): List[Instruction] = {
    instructions.toList
    }

  def refreshInstructions(): Unit = instructions.clear()
  
  private def getSize(ast: ASTNode): Int = {
    ast match {
      case IntLiter(_) => return INT_SIZE
         
      case BoolLiter(_) => return BOOL_SIZE

      case CharLiter(_) => return CHAR_SIZE

      case StringLiter(value) => return STRING_SIZE
      
      case PairLiter => return PAIR_SIZE

      case n@Ident(value) => return identMap.get(n).size

      case ArrLiter(e, es) => 
        
        e match {
          case StringLiter("empty") => return 0
          case _=> 
            val sizes = (e::es).map(elem => getSize(elem))
          return sizes.sum + ARRAY_ELEM_SIZE

        }
      case CallRValue(func, args) =>
        return identMap.get(func).get.size

      case FstPairElem(values) => return -1

      case SndPairElem(values) => return -1
      
      case ArrElem(name, value) =>
        //look up name in identMap
        //get size
        return identMap.get(name).size

      case BaseType(name) =>
        name match {
          case "int" => return INT_SIZE
          case "bool" => return BOOL_SIZE
          case "char" => return CHAR_SIZE
          case "string" => return STRING_SIZE
        }
      case ArrayType(elementType) => return ARRAY_TYPE_SIZE

      case PairType(first, second) => return PAIR_SIZE

      case _ => return EMPTY_SIZE
    }
  }

  def revertTempRegs():Unit = {
    used_TempRegs.clear()
    unused_TempRegs = mutable.ListBuffer() ++ unused_TempRegs_copy
  }

  def revertResultRegs():Unit = {
    used_ResultRegs.clear()
    unused_ResultRegs = mutable.ListBuffer() ++ unused_GeneralRegs_copy
  }

  def pushAndPopx8(size: Int):Unit = {
    instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-size)), ImmVal(0), true))
    instructions.append(I_LoadPair(x8, xzr, Content(sp), ImmVal(size)))
    instructions.append(I_Move(x8, x8))
  }

  def pushUsedRegs(regs: List[Register], noOfVar: Int):Unit = {
    var first = true
    if (noOfVar != 0 && noOfVar != 1){
      for (i <- 0 to noOfVar - 2 by 2) {
        if (first) {
          instructions.append(I_StorePair(regs(i), regs(i+1), Content(sp, ImmVal(-getPointer(noOfVar))), ImmVal(0), true))
          first = false
        } else {
          instructions.append(I_StorePair(regs(i), regs(i+1), Content(sp, ImmVal(16 * (i/2))), ImmVal(0), false))
        }
      }
    }
    if (noOfVar == 1) {
      instructions.append(I_StorePair(regs(noOfVar - 1), xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
    }
    else if (noOfVar%2 == 1) {
      instructions.append(I_StorePair(regs(noOfVar - 1), xzr, Content(sp, ImmVal(math.floorDiv(noOfVar, 2) * 16)), ImmVal(0), false))
    }
  }

  def getPointer (noOfVar: Int): Int = {
    if (noOfVar > 8){
      return 80
    } else {
      return math.ceil(noOfVar.toDouble/2).toInt * 16
    }
  }

  def popUsedRegs(registers: List[Register], n: Int):Unit = {
    var noOfVar = n
    var regs = mutable.ListBuffer().appendAll(registers)
    if (regs.length >= 2) {
      regs.remove(0)
      regs.remove(0)
      noOfVar -= 2
    }
    var noOfLoop = 0
    if (noOfVar != 0 && noOfVar != 1){
      for (i <- 0 to noOfVar - 2 by 2) {
        instructions.append(I_LoadPair(regs(i), regs(i+1), Content(sp, ImmVal(16 * ((i/2)+1)))))
        noOfLoop = i
      }

    }
    if (noOfVar%2 == 1){
      if (n == 1) {
        instructions.append(I_LoadPair(regs.last, xzr, Content(sp), ImmVal((math.floorDiv(noOfVar, 2) + 1) * 16)))
      }
      else {
        instructions.append(I_LoadPair(regs.last, xzr, Content(sp, ImmVal((math.floorDiv(noOfVar, 2) + 1) * 16))))
      }
      
    }
    

    if (registers.length >= 2 && n != 0) {
      instructions.append(I_LoadPair(registers(0), registers(1), Content(sp, ImmVal(0)), ImmVal((math.ceil(noOfVar.toDouble/ 2).toInt + 1) * 16)))
    }
  }

  

  // Helper function to append branch link instruction with a given label name
  def branchLink(s: String): Unit = {
      instructions.append(I_BranchLink(I_Label(s)))
  }

  def getRegFromMap(name: Ident): Register = {
      identMap.get(name) match {
        case Some(value) => return value.reg
        case None => "no such Ident in map"
      }
      return xzr
  }


  def getFuncRegFromMap(name: Ident): Register = {
      funcIdentMap.get(name) match {
        case Some(value) => return value.reg
        case None => "no such Ident in map"
      }
      return xzr
  }

  def getFuncFromMap(name: Ident): ParamList = {
      funcMap.get(name) match {
        case Some(value) => return value
        case None => "no such Func in map"
      }
      return ParamList(List())
  }

  def getIdent(lvalue: LValue): Ident = {
    lvalue match {
      case n@Ident(value) => return n
      case n@ArrElem(name, value) => return name
      case n@FstPairElem(values) => return getIdent(values)
      case n@SndPairElem(values) => return getIdent(values)
    }
  }

  // Helper function:  Jump to overflow handler if overflow occurs
  def checkOverflowHandler(): Unit = { 
    instructions.append(I_Branch(I_Label(ERR_OVERFLOW_LABEL), VS))
    instructions.append(I_Sxtw(x8,x8.toW()))

  }
}
