package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection._
import instruction._
import scala.annotation.unused
import Constant._
import conditions._
import Labels._

object CodeGenerator {

  // For functions specifically: size = func.returnType's size, pointer = func.paramListType.length
  case class identMapEntry(size: Int, pointer: Int)

  private val identMap = mutable.Map[Ident, identMapEntry]()
  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()

  private val pc = 0
  private var labelCounter = 0



  /* 
    generateInstructions (recursive function)
   */
  def generateInstructions(ast: ASTNode): Unit = ast match {

    /* --------------------------- generate program */
    case Program(functions, statements) =>
      
      // Start with the data section
      instructions ++=
        List(I_Directive("data"),
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
      // TODO : generate code for pushing all used regs onto stack (symbolTable)
      instructions.append(I_Move(fp, Content(sp, ImmVal(0))))
      generateInstructions(statements)
      instructions.append(I_Move(x0, ImmVal(0)))
      // generate code for poping all used regs from stack (symbolTable)
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
      instructions.append(I_Ret)

      // Generate code for functions
      for(func <- functions) {
        generateInstructions(func)
      }


      // Add all string labels of the program before all instructions
      if (allDataMsgs.nonEmpty) {

        /*
          Add the DataMsg.instruction of all (String, DataMsg) pairs in the hashmap before all other instructions
        */
        allDataMsgs.flatMap(kv => kv._2.instruction) ++=: instructions

        // Add .data directive to the very top of program
        I_Directive("data") +=: instructions

      }

      


    case Func(returnType, functionName, params, body) =>

      // Generate code for function body
      instructions.append(I_Label(functionName.value))
      instructions.append(I_StorePair(fp, lr, Content(sp, ImmVal(-16)), ImmVal(0), true))
      // TODO : generate code for pushing all used regs onto stack (symbolTable)
      instructions.append(I_Move(fp, Content(sp, ImmVal(0))))
      generateInstructions(body)
      // TODO : enerate code for poping all used regs from stack (symbolTable)
      instructions.append(I_LoadPair(fp, lr, Content(sp, ImmVal(0)), ImmVal(16), false))
      instructions.append(I_Ret)

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

    // case And(expr1, expr2) =>
    //   // bool x = expr1 && (1 && 0)
    //   // x8 : expr1 
    //   // x8 
    //   generateFirst(expr1) 
    //   generateInstructions(expr2)

    //   instructions.append(I_Cmp(used_TempRegs.head, x8))
    //   instructions.append(I_CSet(x8, NE))

      
    //   generateInstructions(expr1, unusedRegs,usedRegs)
    //   generateInstructions(expr2, unusedRegs,usedRegs ++ List(unusedRegs.head))
    //   instructions.append(I_And(usedRegs.head, usedRegs.head, unusedRegs.head))

    // case Or(expr1, expr2) =>
    //   generateInstructions(expr1, unusedRegs,usedRegs)
    //   generateInstructions(expr2, unusedRegs,usedRegs ++ List(unusedRegs.head))
    //   instructions.append(I_Orr(usedRegs.head, usedRegs.head, unusedRegs.head))

  //   case Invert(expr) =>
  //     generateInstructions(expr, unusedRegs, usedRegs) 
  //     instructions.append(I_Xor(x8, x8, ImmVal(1)))

  //   case Negate(expr) =>
  //     generateInstructions(expr, unusedRegs, usedRegs)
  //     instructions.append(I_Neg(x8, x8, LSL(0)))

  //   case Len(expr) =>
  //     generateInstructions(expr, unusedRegs, usedRegs)
  //     instructions.append(I_Move(usedRegs.head, usedRegs.head))

  //   case Ord(expr) =>
  //     generateInstructions(expr, unusedRegs, usedRegs)
  //     instructions.append(I_Sub(x8, x8, ImmVal(0)))


  //   case Chr(expr) =>   
  //     generateInstructions(expr, unusedRegs, usedRegs)
  //     instructions.append(I_Add(x8, x8, ImmValChar(0)))

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
    
    case Print(expr, newline) =>
    case If(condition, thenStat, elseStat) =>
      generateInstructions(condition)
      
      val label1 = I_Label(s".L$labelCounter")
      labelCounter = labelCounter + 1
      val label2 = I_Label(s".L$labelCounter")
      labelCounter = labelCounter + 1

      instructions.append(I_Branch(label1, EQ))   

      generateInstructions(elseStat)
      instructions.append(I_Branch(label2, HI))

      
      instructions.append(label1)
      generateInstructions(thenStat)

      instructions.append(label2)
      
      
    case While(condition, stat) =>
      val label1 = I_Label(s".L$labelCounter")
      labelCounter = labelCounter + 1
      val label2 = I_Label(s".L$labelCounter")
      labelCounter = labelCounter + 1

      instructions.append(I_Branch(label1, HI))

      instructions.append(label2)
      generateInstructions(stat)

      instructions.append(label1)
      generateInstructions(condition)
      instructions.append(I_Branch(label2, EQ))

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
      val label = Labels.addDataMsg(value)
      instructions.append(I_ADRP(x8, I_Label(label)))
    
    case PairLiter => 
    
    case CallRValue(func, args) =>
      // return address link register
      I_Move(lr, ImmVal(pc))
      
      // put parameter into regs
      for (i <- 0 until args.exprl.length) {
        generateInstructions(args.exprl(i))
        // instructions.append(I_Move(paramRegs(i), x8))
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
      case IntLiter(_) => return 32
         
      case BoolLiter(_) => return 8

      case CharLiter(_) => return 7

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
}
