package wacc

import wacc.ast._
import scala.collection._
import Instruction._
import scala.annotation.unused
import Constant._
import Conditions._
import Labels._
import Utility._
// import Shift._
import javax.swing.UIDefaults.LazyValue

class CodeGenerator (varList: List[Int]) {

  // identMap to store all variables and their size & pointer
  // For functions specifically: size = func.returnType's size
  case class identMapEntry(size: Int, reg: Register)
  case class identMapStackEntry(size: Int, pointer: Int)
  
  // Utility Maps
  private val identMap = mutable.Map[Ident, identMapEntry]()
  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()
  private val identStackMap= mutable.Map[Ident, identMapStackEntry]()
  private var assemblyFcuntions = mutable.Map[String, List[Instruction]]()
  private val funcIdentMap = mutable.Map[Ident, identMapEntry]()
  private val funcMap = mutable.Map[Ident, ParamList]()
  
  // Utility booleans
  private var inFunc = false
  private var funcReturned = false
  private var inLoopBranch = false
  private var funcProcessed = 0
  
  // Constants
  final val MOV_MAX: Int = 65536
  final val MOV_MIN: Int = -65537
  final val number_of_remaining_variable = varList.last - unused_ResultRegs.length
  final val initial_offset = number_of_remaining_variable * STACK_ELEM_SIZE
  var temp_number_of_remaining_variable = number_of_remaining_variable


  /* Generates Instructions (recursive function) */
  def generateInstructions(ast: ASTNode): Unit = ast match {
    
    /* ----------------------------------Program-------------------------------------- */
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
      pushUsedRegs(unused_GeneralRegs_copy, varList.last)
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
        revertResultRegs()
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

    // Generate functions
    case Func(returnType, functionName, params, body) =>
      instructions.append(I_Label("wacc_"+functionName.value))
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
      pushUsedRegs(unused_GeneralRegs_copy, varList(funcProcessed))
      instructions.append(I_Move(fp, sp))
      generateInstructions(body)
      funcProcessed += 1

    /* ----------------------------------Expressions-------------------------------------- */
    case Add(expr1, expr2) =>
      arithmeticFlag = true
      generateInstructions(expr1)

      expr2 match {
        // Need to check for immediate value range
        case IntLiter(value) => 
          val fstReg = allocateTempReg()
          instructions.append(I_Move(fstReg.toW(), x8.toW()))
          // Check and load immediate value
          loadImmediate(value) 
          // Now expr2 is in x8
          instructions.append((I_Add(x8.toW(), fstReg.toW(), x8.toW(), true)))

        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          if (true){
            val fstReg = allocateTempReg()
            generateInstructions(expr2)
            instructions.append((I_Add(x8.toW(), fstReg.toW(), x8.toW(), true)))
          } else {
            pushToStack()
          }
      }
      checkOverflowHandler() 

      
    case Sub(expr1, expr2) => 
      arithmeticFlag = true
      generateInstructions(expr1)

      expr2 match {
        // Need to check for immediate value range
        case IntLiter(value) => 
          val fstReg = allocateTempReg()
          instructions.append(I_Move(fstReg.toW(), x8.toW()))
          // Check and load immediate value
          loadImmediate(value) 
          // Now expr2 is in x8
          instructions.append((I_Sub(x8.toW(), fstReg.toW(), x8.toW(), true)))

        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          if (true){
            val fstReg = allocateTempReg()
            generateInstructions(expr2)
            instructions.append((I_Sub(x8.toW(), fstReg.toW(), x8.toW(), true)))
          } else {
            pushToStack()
        }
      }
      // Branch to overflow handler if condition is set
      checkOverflowHandler()


    case Mul(expr1, expr2) =>
      arithmeticFlag = true
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))

      if (true){
        val fstReg = allocateTempReg()
        generateInstructions(expr2)
        // Use signed multiply smull 
        instructions.append(I_Mul(x8, fstReg.toW(), x8.toW(), true))
        // take the 31st bit, sign extend it to 64 bits
        instructions.append(I_Sbfx(fstReg, x8, ImmVal(31), ImmVal(1)))
        // now take the top 32 bits of the result, shift and sign extend to 64 bits
        instructions.append(I_Cmp_Shift(fstReg, x8, ASR(32)))
      } else {
        pushToStack()
      }
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
          if (true){
            val fstReg = allocateTempReg()

            // Store the quotient in x8
            generateInstructions(expr2)

            // Need to compare x8 with 0 to check for division by zero
            instructions.append(I_Cbz(x8, I_Label(ERR_DIV_ZERO_LABEL))) 
            instructions.append(I_SDiv(x8, fstReg, x8))
          } else {
            pushToStack()
          }
      }
      checkOverflowHandler()

    //create instruction for mod
    case Mod(expr1, expr2) =>
      arithmeticFlag = true
      divByZeroFlag = true
       
      //get mod by expr1 - expr2 * (expr1/ expr2)
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true){
      val fstReg = allocateTempReg()
      generateInstructions(expr2)
      instructions.append(I_Cbz(x8, I_Label(ERR_DIV_ZERO_LABEL)))
      instructions.append(I_SDiv(unused_TempRegs.head, fstReg, x8))
      instructions.append(I_Mul(unused_TempRegs.head, unused_TempRegs.head, x8))
      instructions.append(I_Sub(x8, fstReg, unused_TempRegs.head))
      } else {
        pushToStack()
      }
      checkOverflowHandler()
      
    // Generates instructions for LT
    case LessThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true){
      val fstReg = allocateTempReg()
      generateInstructions(expr2)
      instructions.append(I_Cmp(fstReg, x8))
      } else {
        pushToStack()
      }
      instructions.append(I_CSet(x8, LT))
     
    // Generates instructions for LTE
    case LessThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true) {
      val fstReg = allocateTempReg()
      generateInstructions(expr2)
      instructions.append(I_Cmp(fstReg, x8))
      } else {
        pushToStack()
      }
      instructions.append(I_CSet(x8, LE))

    // Generates instructions for GT
    case GreaterThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true){
        val fstReg = allocateTempReg()
        generateInstructions(expr2)
        instructions.append(I_Cmp(fstReg, x8))
      } else {
        pushToStack()
      }
      instructions.append(I_CSet(x8, GT))

    // Generates instructions for GTE
    case GreaterThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true){
        val fstReg = allocateTempReg()
        generateInstructions(expr2)
        instructions.append(I_Cmp(fstReg, x8))
      } else {
        pushToStack()
      }
      instructions.append(I_CSet(x8, GE))

    // Generates instructions for EQ
    case Eq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true) {
      val fstReg = allocateTempReg()
      generateInstructions(expr2)
      instructions.append(I_Cmp(fstReg, x8))
      } else {
        pushToStack()
      }
      instructions.append(I_CSet(x8, EQ))
    
    // Generates instructions for NE
    case NotEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      if (true){
        val fstReg = allocateTempReg()
        generateInstructions(expr2)
        instructions.append(I_Cmp(fstReg, x8))
      } else {
        pushToStack()
      }
      instructions.append(I_CSet(x8, NE))

    // Generates instructions for AND
    case And(expr1, expr2) =>
      generateInstructions(expr1)
      val label = I_Label(addLabel())
      instructions.append(I_Cmp(x8, ImmVal(1)))
      instructions.append(I_Branch(label, NE))

      generateInstructions(expr2)
      instructions.append(I_Cmp(x8, ImmVal(1)))

      instructions.append(label)
      instructions.append(I_CSet(x8, EQ))
      pushAndPopx8(16)

    // Generates instructions for OR
    case Or(expr1, expr2) =>
      generateInstructions(expr1)
      val label = I_Label(addLabel())
      instructions.append(I_Cmp(x8, ImmVal(1)))
      instructions.append(I_Branch(label, EQ))

      generateInstructions(expr2)
      instructions.append(I_Cmp(x8, ImmVal(1)))

      instructions.append(label)
      instructions.append(I_CSet(x8, EQ))
      pushAndPopx8(16)

    // Generates instructions for INVERT
    case Invert(expr) =>
      generateInstructions(expr)
      instructions.append(I_Cmp(x8, ImmVal(1)))
      instructions.append(I_CSet(x8, NE))
      pushAndPopx8(16)

    // Generates instructions for NEGATE
    case Negate(expr) =>
      // Need to check for overflow if the max negative number is negated to be positive, 
      // which will exceed the upper bound causing overflow
      arithmeticFlag = true
      expr match {
        case IntLiter(value) => 
          loadImmediate(-value)
        case ShortIntLiter(value) =>
          loadImmediate(-value.toInt)
        case ByteIntLiter(value) =>
          loadImmediate(-value.toInt)
        case _ => 
          generateInstructions(expr)
          instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
          instructions.append(I_LoadPair(x9, xzr, Content(sp), ImmVal(16)))
          instructions.append(I_Move(x8, ImmVal(0)))
          val reg = allocateTempReg()
          instructions.append(I_Sub(x8.toW(), x8.toW(), reg.toW(), true))
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
      
    case Len (expr) => 
      expr match {
        case ArrElem(name, value) => 
          identMap.get(name) match {
            case Some(i) => 
              instructions.append(I_Move(x8, ImmVal(i.size / (4 ^ value.length))))
            case None => 
          }
        case n@Ident(value) => 
          identMap.get(n) match {
            case Some(i) => 
              instructions.append(I_Move(x8, ImmVal(i.size / 4)))
            case None => 
          }
        
        case _ => 
      }
    
    case BitAnd(expr1, expr2) => 
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
          if (true){
            val fstReg = allocateTempReg()
            generateInstructions(expr2)
            instructions.append((I_And(x8, fstReg, x8)))
          } else {
            pushToStack()
        }

    case BitOr(expr1, expr2) => 
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
          if (true){
            val fstReg = allocateTempReg()
            generateInstructions(expr2)
            instructions.append((I_Orr(x8, fstReg, x8)))
          } else {
            pushToStack()
        }

    case BitNot(expr) => 
      arithmeticFlag = true
      generateInstructions(expr)
      instructions.append(I_Mvn(x8, x8))
      checkOverflowHandler()

   /* ----------------------------------Statements-------------------------------------- */
    case Read(lValue) =>

        generateInstructions(lValue)
    
        val n = getIdent(lValue)
        
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
        instructions.append(I_Move(getRegFromMap(n, identMap), x8))

      
        
    case NewAssignment(identType, name, value) =>
      generateInstructions(value)
      // Check if variable numbers exceeds the register limit
      if (!checkIfneedStack()) {
        //allocate the value a register and push it in to identMap
        val fstReg = allocateResultReg()
        instructions.append(I_Move(fstReg, x8))
        identMap(name) = identMapEntry(getSize(value), used_ResultRegs.head)
      }
      revertTempRegs()
          

    case Assignment(name, value) => 
      var reg: Register = xzr
      
    // Get functions from identMap (or funcIdentMap if currently in function)
      if (inFunc) {
        reg = getRegFromMap(getIdent(name), funcIdentMap)
        if (reg == xzr) {
          reg = getRegFromMap(getIdent(name), identMap)
        }
      } else {
        reg = getRegFromMap(getIdent(name), identMap)
      }
      
      name match {

        // Special case for arrays
        case n@ArrElem(name, v)=>
          var counter = v.length * ARRAY_ELEM_SIZE
          for (expr <- v) {
            generateInstructions(expr)
            if (counter != ARRAY_ELEM_SIZE) {
              instructions.append(I_Move(x17.toW(), x8.toW()))
              if (counter !=  v.length * ARRAY_ELEM_SIZE) {
                instructions.append(I_LoadPair(x9, xzr, Content(sp), ImmVal(16)))
              }
              instructions.append(I_Move(x7, getRegFromMap(name, identMap)))
              branchLink(s"_arrLoad$counter")
              counter -= ARRAY_ELEM_SIZE
            
              instructions.append(I_Move(x8, x7))
              instructions.append(I_Move(x8, x8))
              instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
            }
            else {
              arrstoreFlag += counter
              var fstReg = getRegFromMap(name, identMap)
              if (v.length != 1) {
                instructions.append(I_LoadPair(unused_TempRegs.head, xzr, Content(sp), ImmVal(16)))
                if(true){
                  val fstReg = allocateTempReg()
                } else {
                  pushToStack()
                }
              }
              instructions.append(I_Move(x17, x8))
              generateInstructions(value)
              instructions.append(I_Move(x7, fstReg))
              branchLink(s"_arrStore$counter")
              
              generateInstructions(n)
            }
          errOutOfBoundFlag = true
        }

        // Special case for pairs
        case n@FstPairElem(v) =>
          handlePairElementCase(reg, value, FSTPAIROFFSET)
        
        case n@SndPairElem(v) => 
          handlePairElementCase(reg, value, SNDPAIROFFSET)

        case _=>
          generateInstructions(name)
          generateInstructions(value)
          
         
        instructions.append(I_Move(reg, x8))
        revertTempRegs() 
      }
      
    
    case Free(expr) => 
      generateInstructions(expr)

      val t = expr.getType
      val isArray = t.contains("[]")

      // Need to adjust pointer to free the referenc address of array    

      if (isArray) {
        instructions.append(I_Sub(x8, x19, ImmVal(4)))
        pushAndPopx8(16)
      } 
      instructions.append(I_Move(x0, x8))
      
      if (isArray) {
        branchLink(FREE_LABEL)
      } else {
        branchLink(FREE_PAIR_LABEL)
        freePairFlag = true
      }
      nullPointerFlag = true

    case Return(expr) => 

      // Check if return keyword is in loop or already returned in function
      if (!funcReturned || inLoopBranch){
        generateInstructions(expr)
        instructions.append(I_Move(x0, x8)) 
        popUsedRegs(used_ResultRegs.toList.reverse, varList(funcProcessed))
        instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16)))
        instructions.append(I_Ret)
        if(!inLoopBranch) {
          funcReturned = true
        }
      }
      
    
    case Exit(expr) =>
      generateInstructions(expr)
      instructions.append(I_Move(x0, x8))
      branchLink(EXIT_LABEL)

    

    case Print(expr, newline) =>

      if (inFunc){
        instructions.append(I_StorePair(x0, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instructions.append(I_Move(x16, sp))
      }

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

        case ("int") =>
          printIntFlag = true
          branchLink(PRINT_INT_LABEL)

        case _ =>
          if (expr.getType.startsWith("int") & !expr.getType.contains("[]")) {
            printIntFlag = true
            branchLink(PRINT_INT_LABEL)
          } else {
            printPFlag = true
            branchLink(PRINT_P_LABEL)
          }
          
        
      }
  
      if (newline) {
        printLineFlag = true
        branchLink( PRINT_LN_LABEL)
      }
      
      if(inFunc){
        instructions.append(I_LoadPair(x0, xzr, Content(sp), ImmVal(16)))
      }
      

    
    case If(condition, thenStat, elseStat) =>
      inLoopBranch = true
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
          instructions.append(I_Branch(I_Label(if_then), EQ))
        case Or(expr1, expr2) => 
          instructions.append(I_Branch(I_Label(if_then), EQ))
        case BoolLiter(value) => 
          if (value) {
            instructions.append(I_Branch(I_Label(if_then), NE))
          } else {
            instructions.append(I_Branch(I_Label(if_then), EQ))
          }
        case n@Ident(value) =>
          generateInstructions(n)
          instructions.append(I_Cmp(x8, ImmVal(1)))
          instructions.append(I_Branch(I_Label(if_then), EQ))
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

      inLoopBranch = false

    case While(condition, stat) =>

      inLoopBranch = true
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
      instructions.append(I_Cmp(x8, ImmVal(0)))
      
      // Jump back to body of while if condition holds
      instructions.append(I_Branch(I_Label(while_body), NE))
      inLoopBranch = false
      
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

      instructions.append(I_Store(x8, Content(x16, ImmVal(8))))

      instructions.append(I_Move(x8, x16))


    /* ----------------------------------Literals-------------------------------------- */
    

    case IntLiter(value)=> 
      // Call helper function to check and load immediate value
      loadImmediate(value)
         
    case ShortIntLiter(value) => 
      loadImmediate(value.toInt)

    case ByteIntLiter(value) => 
      loadImmediate(value.toInt)

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
      
    //create instruction for arrayElem
    case ArrElem(name, value) => 
      //use counter to remember the size of arrLoad
      var counter = value.length * 4
      for (expr <- value) {
        arrloadFlag += counter
        generateInstructions(expr)
        instructions.append(I_Move(x17.toW(), x8.toW()))
        instructions.append(I_Move(x7, getRegFromMap(name, identMap)))
        branchLink(s"_arrLoad$counter")
        instructions.append(I_Move(x8.toW(), x7.toW()))
        instructions.append(I_Move(x8, x8))
        counter -= 4
        //push dx8 and pop to x9 if is not the last loop
        if (counter != 0) {
          instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
          instructions.append(I_Move(x7, ImmVal(1)))
          instructions.append(I_LoadPair(x9, xzr, Content(sp), ImmVal(16)))
        }
        errOutOfBoundFlag = true
      }
    
    case PairLiter => 
      instructions.append(I_Move(x8, ImmVal(0)))
      
    //create instructions for ArrLiter
    case a@ArrLiter(e, es) =>
      instructions.append(I_Move(x0, ImmVal(getSize(a))))
      branchLink(MALLOC_LABEL)
      instructions.append(I_Move(x16, x0))
      instructions.append(I_Add(x16, x16, ImmVal(ARRAY_ELEM_SIZE)))

      // Size of array  
      var arrSize = 0
      e match {
        case StringLiter("empty") => arrSize = 0
        case _=> arrSize = (es.length + 1)
      }
      instructions.append(I_Move(x8, ImmVal(arrSize)))

      instructions.append(I_Store(x8, Content(x16, ImmVal(-ARRAY_ELEM_SIZE))))
      //push the element in array to stack one by one
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
      // Pushes the current value in x0 
      if (inFunc) {
        instructions.append(I_StorePair(x0, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
        instructions.append(I_Move(x16, sp))
      }

      for (i <- 0 until args.exprl.length) {
        generateInstructions(args.exprl(i))
        instructions.append(I_Move(unused_ParamRegs(i), x8))
        val p = getFuncFromMap(func).paramListType(i)
        funcIdentMap(p.paramName) = identMapEntry(getSize(p.paramType), unused_ParamRegs(i))
      }
      branchLink( "wacc_" + func.value)
      instructions.append(I_Move(x16, x0))
      // Pop back what was used to be in x0
      if (inFunc) {
        instructions.append(I_LoadPair(x0, xzr, Content(sp), ImmVal(16)))
      }

      instructions.append(I_Move(x8, x16))

    //add _errNull, get fst by pop from the resiter with offset 0
    case FstPairElem(values) =>
      val fstPairelem : Option[FstPairElem] = Some(FstPairElem(values))

      // val reg = getRegFromMap(getIdent(values), identMap)
      // instructions.append(I_Cbz(reg, I_Label("_errNull")))
      // instructions.append(I_Load(x8, Content(sp, ImmVal(0))))

      // nullPointerFlag = true
      fstPairelem match {
        case Some(FstPairElem(values)) => 
          val reg = getRegFromMap(getIdent(values), identMap)
          instructions.append(I_Cbz(reg, I_Label("_errNull")))
          instructions.append(I_Load(x8, Content(reg, ImmVal(0))))
          // ToDO : handle null pointer is false, do not need to throw error
          nullPointerFlag = true // should be false
        case None =>   nullPointerFlag = true
      }
      
    //add _errNull, get fst by pop from the resiter with offset 8
    case SndPairElem(values) => 
      val sndPairelem : Option[SndPairElem] = Some(SndPairElem(values))

      sndPairelem match {
        case Some(SndPairElem(values)) => 
          val reg = getRegFromMap(getIdent(values), identMap)
          instructions.append(I_Cbz(reg, I_Label("_errNull")))
          instructions.append(I_Load(x8, Content(reg, ImmVal(8))))
          nullPointerFlag = true
        case None =>   nullPointerFlag = true
      }
      // val reg = getRegFromMap(getIdent(values), identMap)
      // instructions.append(I_Cbz(reg, I_Label("_errNull")))
      // instructions.append(I_Load(x8, Content(sp, ImmVal(8))))

      // nullPointerFlag = true

    case Ident(value) => 
      // Get functions from identMap (or funcIdentMap if currently in function)
      if (inFunc) {
        var reg: Register = getRegFromMap(Ident(value), funcIdentMap)
        if (reg == xzr) {
          reg = getRegFromMap(Ident(value),identMap)
        }
        instructions.append(I_Move(x8, reg))

      } else {
        instructions.append(I_Move(x8, getRegFromMap(Ident(value), identMap)))
      }
      

    case _ => 
      
  }


  /* ------------------- Helper functions ------------------- */
  def getInstructions(): List[Instruction] = {
    instructions.toList
    }

  def refreshInstructions(): Unit = instructions.clear()
  
  def getSize(ast: ASTNode): Int = {
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
            val sizes = ((e::es).length) * 4
          return sizes + ARRAY_ELEM_SIZE

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

  // Used after arithmetic operations
  def pushAndPopx8(size: Int):Unit = {
    instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-size)), ImmVal(0), true))
    instructions.append(I_LoadPair(x8, xzr, Content(sp), ImmVal(size)))
    instructions.append(I_Move(x8, x8))
  }

  // Pushes all registers that 
  def pushUsedRegs(regs: List[Register], n: Int):Unit = {
    var noOfVar = n
    if (noOfVar > regs.length) noOfVar = 0
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

  // Used to calculate pointer when pushing/popping registers
  def getPointer (noOfVar: Int): Int = {
    if (noOfVar > 8){
      return 80
    } else {
      return math.ceil(noOfVar.toDouble/2).toInt * 16
    }
  }

  // Pops all registers that is used during the program/function
  def popUsedRegs(registers: List[Register], n: Int):Unit = {
    var noOfVar = n
    if (noOfVar > registers.length) noOfVar = 0
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

  // Gets the corresponding register from the specified map
  def getRegFromMap(name : Ident, map: mutable.Map[Ident,identMapEntry]) : Register = {
      map.get(name) match {
        case Some(value) => return value.reg
        case None => "no such Ident in map"
      }
      xzr
  }

  // Gets the corresponding function from the function map
  def getFuncFromMap(name: Ident): ParamList = {
      funcMap.get(name) match {
        case Some(value) => return value
        case None => "no such Func in map"
      }
      ParamList(List())
  }

  // Gets the ident given an lValue
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

  def loadImmediate(value: Int) : Unit = {
    if (value > MOV_MAX) {
      // Cannot load in one single instruction
      if (true){
        val fstReg = allocateTempReg()
        // Use two separate instructions to load
        instructions.append(I_Move(fstReg, ImmVal(value & 0xFFFF)))
        instructions.append(I_Movk(fstReg, ImmVal(value >> 16), LSL(16)))
        ImmVal(value & 0xFFFF)

        instructions.append(I_Move(x8, fstReg)) 
      } else {
        pushToStack()
      }
    } else {
      instructions.append(I_Move(x8, ImmVal(value)))
    }
  }

  def handlePairElementCase(reg: Register, value : RValue, offset: Int): Unit = {
    nullPointerFlag = true
    instructions.append(I_Cbz(reg, I_Label(ERR_NULL_LABEL)))
    generateInstructions(value)
    instructions.append(I_Store(x8, Content(reg, ImmVal(offset))))
  }

    /* ---------------- choose STACK  or Reg  -------------------------------------- */
   /*  handle the case when run out of register then we need to push to stack */
    def allocateTempReg() : Reg = {
    used_TempRegs = unused_TempRegs.head +: used_TempRegs
    val fstReg = used_TempRegs.head
    unused_TempRegs.remove(0)
    fstReg
  }

  def allocateResultReg() : Reg = {
    used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs
    val fstReg = used_ResultRegs.head
    unused_ResultRegs.remove(0)
    fstReg
  }
  
  def checkIfneedStack() : Boolean = {
    var result = false 
    if (varList.last > unused_ResultRegs.length) {
      result = true
    }
    result
  }

  def pushToStack() : Unit = {
    var offset = temp_number_of_remaining_variable * STACK_ELEM_SIZE
    instructions.append(I_Move(x17, ImmVal(-offset)))
    instructions.append(I_Store(x8.toW(), Content(fp, x17)))
    temp_number_of_remaining_variable = temp_number_of_remaining_variable - 1 
  }  

}
