package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection._
import instruction._
import scala.annotation.unused
import Constant._
import conditions._
import Labels._

class CodeGenerator (varList: List[Int]) {

  // identMap to store all variables and their size & pointer
  // For functions specifically: size = func.returnType's size, pointer = func.paramListType.length
  case class identMapEntry(size: Int, pointer: Int)

  private val identMap = mutable.Map[Ident, identMapEntry]()
  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()

  private var funcProcessed = 0
  
  /* 
    generateInstructions (recursive function)
  */
  def generateInstructions(ast: ASTNode): Unit = ast match {
    /* --------------------------- generate program */
    case Program(functions, statements) =>
      

      // Start with the data section
      instructions ++= List(
        I_Directive("align 4"),
        I_Directive("text"),
        I_Directive("global main"), 
        I_Label("main"))
      
      // Add function names to identMap
      for(func <- functions) {
        identMap(Ident("wacc_" + func.functionName.value)) = identMapEntry(getSize(func.returnType), func.params.paramListType.length)
      }

      // Generate code for main body
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
      pushUsedRegs(unused_ResultRegs.toList, varList.last)
      instructions.append(I_Move(fp, Content(sp, ImmVal(0))))
      generateInstructions(statements)
      instructions.append(I_Move(x0, ImmVal(0)))
      popUsedRegs(unused_ResultRegs.toList, varList.last)
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
      instructions.append(I_Ret)

      // Generate code for functions
      for(func <- functions) {
        generateInstructions(func)
      }


      // Add all string labels at the top of program
      if (allDataMsgs.nonEmpty) {

        // Add the DataMsg.instruction of all (String, DataMsg) pairs in the hashmap before all other instructions
        allDataMsgs.flatMap(kv => kv._2.instruction) ++=: instructions

        // Add .data directive to the very top of program
        I_Directive("data") +=: instructions

      }

      


    case Func(returnType, functionName, params, body) =>

      instructions.append(I_Label(functionName.value))
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
      pushUsedRegs(unused_ResultRegs.toList, varList(funcProcessed))
      instructions.append(I_Move(fp, Content(sp, ImmVal(0))))
      generateInstructions(body)
      popUsedRegs(unused_ResultRegs.toList, varList(funcProcessed))
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
      instructions.append(I_Ret)
      funcProcessed += 1

    /* --------------------------- generate expressions */
    case Add(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Add(x8, x8, ImmVal(value)))
        case _=> 
          // int x = 1- (2 - 1)
          // mov x9 x8 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Add(x8, used_TempRegs.head, x8))
      }
      revertTempRegs()

      
    case Sub(expr1, expr2) => 
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Sub(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Sub(x8, used_TempRegs.head, x8))
      }

      

    case Mul(expr1, expr2) =>
      generateInstructions(expr1)  // mov x8 expr1
      instructions.append(I_Move(unused_TempRegs.head, x8))  // mov x8 x9 
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)  //mov x8 epr2 
      
      instructions.append(I_Mul(x8, used_TempRegs.head, x8))  // mul x8 x9 x8
      revertTempRegs()


    case Div(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_UDiv(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_UDiv(x8, used_TempRegs.head, x8))
      }

    case Mod(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)
      
      // x8 = expr2, used_TempRegs.head = expr2
      
      // UDIV unused_TempRegs.head, x8, used_TempRegs.head  
      // MUL  unused_TempRegs.head, unused_TempRegs.head, used_TempRegs.head   
      // SUB  x8, x8, unused_TempRegs.head
      instructions.append(I_UDiv(unused_TempRegs.head, used_TempRegs.head, x8))
      instructions.append(I_Mul(unused_TempRegs.head, unused_TempRegs.head, x8))
      instructions.append(I_Sub(x8, used_TempRegs.head, unused_TempRegs.head))


    case LessThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, LT))
      //push and pop x8
        
    case LessThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, LE))

    case GreaterThan(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GT))

    case GreaterThanEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GE))

    case Eq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, EQ))
    
    case NotEq(expr1, expr2) =>
      generateInstructions(expr1)
      instructions.append(I_Move(unused_TempRegs.head, x8))
      used_TempRegs = unused_TempRegs.head +: used_TempRegs
      unused_TempRegs.remove(0)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
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
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_And(x8, used_TempRegs.head, x8))
      }

    case Or(expr1, expr2) =>
      generateInstructions(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Orr(x8, x8, ImmVal(value)))
        case _=> 
          instructions.append(I_Move(unused_TempRegs.head, x8))
          used_TempRegs = unused_TempRegs.head +: used_TempRegs
          unused_TempRegs.remove(0)
          generateInstructions(expr2)
          instructions.append(I_Orr(x8, used_TempRegs.head, x8))
      }

    case Invert(expr) =>
      generateInstructions(expr)
      instructions.append(I_Cmp(x8, ImmVal(1)))
      instructions.append(I_CSet(x8, NE))
      //push and pop x8
      pushAndPopx8(16)
      


    case Negate(expr) =>
      generateInstructions(expr)
      pushAndPopx8(16)

  //   case Len(expr) =>
  //     generateInstructions(expr, unusedRegs, usedRegs)
  //     instructions.append(I_Move(usedRegs.head, usedRegs.head))

    case Ord(expr) =>
      generateInstructions(expr)


    case Chr(expr) =>   
      generateInstructions(expr)
      //check the number of char is in ASCII table

      pushAndPopx8(16)

   // -------------------------- Generate instructions for statements
    case Read(lvalue) =>
        
    case NewAssignment(identType, name, value) => 
      identMap(name) = identMapEntry(getSize(value),0 /*current pointer*/)
      value match {
        case ArrLiter(e, es) => 
          val ess = e+:es
          for (expr <- ess) {
            generateInstructions(e)
            instructions.append(I_Store(used_TempRegs.head, sp, false))
            instructions.append(I_Add(sp, sp, ImmVal(getSize(expr))))
          }
        
        case NewPairRValue(exprL, exprR) =>
          val size = getSize(exprL) + getSize(exprR)
          instructions.append(I_Move(x0 /* should be head of  */, ImmVal(size)))
          instructions.append((I_BranchLink(I_Label("malloc"))))
          instructions.append(I_Move(x16 /* should be implemented to temp reg head*/, x0))


          generateInstructions(exprL)
          instructions.append(I_Move(x8, unused_TempRegs.head))
          instructions.append(I_Store(x8, Content(x16/* should be implemented to temp reg head*/, ImmVal(0)), false))
          

          generateInstructions(exprR)
          instructions.append(I_Move(x8, unused_TempRegs.head))
          instructions.append(I_Store(x8, Content(x16/* should be implemented to temp reg head*/, ImmVal(getSize(exprL))), false))

          instructions.append(I_Move(x8, x16 /* should be implemented to temp reg head*/))
          instructions.append(I_Move(unused_TempRegs.head, x8))
          
        case _ => 
          generateInstructions(value)
          instructions.append(I_Move(unused_ResultRegs.head, x8))
          used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
          unused_ResultRegs.remove(0)

          
      }
      

    case Assignment(name, value) => 
    
    case Free(expr) => 
    case Return(expr) => 
      generateInstructions(expr)
      instructions.append(I_Move(x0, used_ResultRegs.head))
      
    case Exit(expr) =>
      generateInstructions(expr)
      instructions.append(I_Move(unused_ParamRegs.head, x8))
      instructions.append(I_BranchLink(I_Label("exit")))

    
    case Print(expr, newline) =>
      generateInstructions(expr)

      // instructions.append(I_ADRP()

    
    
    case If(condition, thenStat, elseStat) =>
      generateInstructions(condition)
      val (if_then, if_end) = addIfLabel()

      // Branch instruction, Jump to then clause if condition is EQ, otherwise continue to else clause
      instructions.append(I_Branch(I_Label(if_then), EQ)) 

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
      instructions.append(I_Branch(I_Label(while_body), EQ))
      
    case Begin(stmt) =>
      generateInstructions(stmt)
    
    case SeqStmt(first, second) => 
      generateInstructions(first)
      generateInstructions(second)
        
    case Skip =>
    

    case IntLiter(value)=> instructions.append(I_Move(x8, ImmVal(value)))
         
    case BoolLiter(value) => 
      value match {
        case true => instructions.append(I_Move(x8, ImmVal(1)))
        case false => instructions.append(I_Move(x8, ImmVal(0)))
      }

    case CharLiter(value) => instructions.append(I_Move(x8, ImmValChar(value)))

    case StringLiter(value) => 
      // Create label string with the given string 
      val label = Labels.addDataMsg(value)
      // Convert the label string to an I_Label class and store at x8
      instructions.append(I_ADRP(x8, I_Label(label)))
    
    case PairLiter => 

    case ArrLiter(e, es) => 

    
    case CallRValue(func, args) =>
      for (i <- 0 until args.exprl.length) {
        generateInstructions(args.exprl(i))
        instructions.append(I_Move(unused_ParamRegs(i), x8))
      }
      instructions.append(I_BranchLink(I_Label("wacc_" + func.value)))
      instructions.append(I_Move(x16, x0))
      instructions.append(I_Move(x8, x16))

    case _ => 
      
  }


  /* ------------------- Helper functions ------------------- */
  def getInstructions(): List[Instruction] = {
    instructions.toList
    }

  def refreshInstructions(): Unit = instructions.clear()
  
  private def getSize(ast: ASTNode): Int = {
    ast match {
      case IntLiter(_) => return 4
         
      case BoolLiter(_) => return 1

      case CharLiter(_) => return 4

      case StringLiter(value) => return 32
      
      case PairLiter => return 32

      case Ident(value) => return 0 //

      case ArrLiter(e, es) => return 32

      case CallRValue(func, args) =>
        if(!identMap.contains(func)) {
          return -1
        }
        return identMap.get(func).get.size

      case FstPairElem(values) => return -1

      case SndPairElem(values) => return -1
      
      case ArrElem(name, value) =>
        //look up name in identMap
        //get size
        return -1

      case BaseType(name) =>
        name match {
          case "int" => return 32
          case "bool" => return 8
          case "char" => return 7
          case "string" => return 32
        }
      case ArrayType(elementType) => return 32

      case PairType(first, second) => return 32

      case _ => return -1
    }
  }


  /*  
    1. generate first expression
    2. store the result in x8
    3. move x8 to unused_ResultRegs.head (x9) 
  */
  def generateFirst(expr1: Expr): Unit = {
    generateInstructions(expr1)
    instructions.append(I_Move(unused_TempRegs.head, x8)) 
    used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
    unused_ResultRegs.remove(0)
  }

  def revertTempRegs():Unit = {
    used_TempRegs.clear()
    unused_TempRegs = mutable.ListBuffer() ++ unused_TempRegs_copy
    // print(used_TempRegs)
    // print(unused_TempRegs_copy)
    // print(unused_TempRegs)
  }

  def pushAndPopx8(size: Int):Unit = {
    instructions.append(I_StorePair(x8, xzr, Content(sp, ImmVal(-size)), ImmVal(0), true))
    instructions.append(I_LoadPair(x8, xzr, Content(sp), ImmVal(size)))
    instructions.append(I_Move(x8, x8))
  }

  def pushUsedRegs(regs: List[Register], noOfVar: Int):Unit = {
    for (i <- 0 to noOfVar by 2) {
      instructions.append(I_StorePair(regs(i), regs(i+1), Content(sp, ImmVal(-16)), ImmVal(0), true))
    }

    if (noOfVar%2 == 1) {
      instructions.append(I_StorePair(regs.last, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))
    }
  }

  def popUsedRegs(regs: List[Register], noOfVar: Int):Unit = {
    for (i <- 0 to noOfVar by 2) {
      instructions.append(I_LoadPair(regs(i), regs(i+1), Content(sp, ImmVal(0)), ImmVal(16), false))
    }

    if (noOfVar%2 == 1) {
      instructions.append(I_LoadPair(regs.last, xzr, Content(sp, ImmVal(0)), ImmVal(16), false))
    }
  }


  //   _malloc:
  // 54		// push {lr}
  // 55		stp lr, xzr, [sp, #-16]!
  // 56		bl malloc
  // 57		cbz x0, _errOutOfMemory
  // 58		// pop {lr}
  // 59		ldp lr, xzr, [sp], #16
  // 60		ret
  // 61	
  // 62	// length of .L._errOutOfMemory_str0
  // 63		.word 27
  // 64	.L._errOutOfMemory_str0:
  // 65		.asciz "fatal error: out of memory\n"
  // 66	.align 4
  // 67	_errOutOfMemory:
  // 68		adr x0, .L._errOutOfMemory_str0
  // 69		bl _prints
  // 70		mov w0, #-1
  // 71		bl exit
  def malloc():Unit = {
    instructions.append(I_Label("_malloc"))

    instructions.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

    instructions.append(I_BranchLink(I_Label("malloc")))
    instructions.append(I_CBZ(x0, I_Label("_errOutOfMemory")))
    
    instructions.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
    instructions.append(I_Ret)
    errOutOfMemory()
  }

  def errOutOfMemory():Unit = {
    
    addCustomisedDataMsg("fatal error: out of memory\n", "_errOutOfMemory_")

    instructions.append(I_Directive("align 4"))

    instructions.append(I_Label("_errOutOfMemory"))

    instructions.append(I_ADR(x0, I_Label(".L._errOutOfMemory_str0")))

    instructions.append(I_BranchLink(I_Label("_prints")))

    instructions.append(I_Move(x0, ImmVal(-1)))

    instructions.append(I_BranchLink(I_Label("exit")))
  }

  //   	.word 4
  // 37	.L._prints_str0:
  // 38		.asciz "%.*s"
  // 39	.align 4
  // 40	_prints:
  // 41		// push {lr}
  // 42		stp lr, xzr, [sp, #-16]!
  // 43		mov x2, x0
  // 44		ldrsw x1, [x0, #-4]
  // 45		adr x0, .L._prints_str0
  // 46		bl printf
  // 47		mov x0, #0
  // 48		bl fflush
  // 49		// pop {lr}
  // 50		ldp lr, xzr, [sp], #16
  // 51		ret
  def prints():Unit = {
    addCustomisedDataMsg("%.*s", "_prints_")

    instructions.append(I_Directive("align 4"))

    instructions.append(I_Label("_prints"))

    instructions.append(I_StorePair(lr, xzr, Content(sp, ImmVal(-16)), ImmVal(0), true))

    instructions.append(I_Move(x2, x0))
    instructions.append(I_LDRSW(x1, Content(x0, ImmVal(-4))))
    instructions.append(I_ADR(x0, I_Label(".L._prints_str0")))

    instructions.append(I_BranchLink(I_Label("printf")))
    instructions.append(I_Move(x0, ImmVal(0)))

    instructions.append(I_BranchLink(I_Label("fflush")))
    
    instructions.append(I_LoadPair(lr, xzr, Content(sp, ImmVal(16)), ImmVal(0), false))
    instructions.append(I_Ret)
  }



  
}
