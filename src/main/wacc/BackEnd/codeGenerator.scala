package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection._
import conditions._
import shift._
import Constant._
import scala.annotation.unused
object CodeGenerator {

  case class identMapEntry(size: Int, pointer: Int)
  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()
  private val identMap = mutable.Map[Ident, identMapEntry]()   // Map(ident_name, stack_pointer)

  private val pc = 0
  private var stringCounter = 0



  /* 
    generateInstructions (recursive function)
   */
  def generateInstructions(ast: ASTNode): Unit = ast match {

    /* --------------------------- generate program */
    case Program(functions, statements) =>


    case Func(returnType, functionName, params, body) =>
      instructions.append(I_Label(functionName.value))
      identMap(functionName) = identMapEntry(getSize(returnType), params.paramListType.length)
      generateInstructions(body)
      instructions.append(I_Ret)

    /* --------------------------- generate expressions */
    case Add(expr1, expr2) =>
     generateFirst(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Add(x8, used_TempRegs.head, ImmVal(value)))
        case _=> 
          generateInstructions(expr2)
          instructions.append(I_Add(x8, used_TempRegs.tail.head, used_TempRegs.head))
      }
      instructions.append(I_Move(unused_ResultRegs.head, x8))
      used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
      unused_ResultRegs.remove(0)
      
    case Sub(expr1, expr2) => 
      generateFirst(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_Sub(x8, used_TempRegs.head, ImmVal(value)))
        case _=> 
          generateInstructions(expr2)
          instructions.append(I_Sub(x8, used_TempRegs.tail.head, used_TempRegs.head))
      }
      instructions.append(I_Move(unused_ResultRegs.head, x8))
      used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
      unused_ResultRegs.remove(0)
      
      

    case Mul(expr1, expr2) =>
      generateFirst(expr1)

      generateInstructions(expr2)
      instructions.append(I_Mul(x8, used_TempRegs.tail.head, used_TempRegs.head))
      instructions.append(I_Move(unused_ResultRegs.head, x8))
      used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
      unused_ResultRegs.remove(0)

    case Div(expr1, expr2) =>
      generateFirst(expr1)
      
      expr2 match {
        case IntLiter(value) => 
          instructions.append(I_UDiv(x8, used_TempRegs.head, ImmVal(value)))
        case _=> 
          generateInstructions(expr2)
          instructions.append(I_UDiv(x8, used_TempRegs.tail.head, used_TempRegs.head))
      }

    case Mod(expr1, expr2) =>
      generateFirst(expr1)
      generateInstructions(expr2)
      
      // x8 = expr2, used_TempRegs.head = expr2
      
      // UDIV unused_TempRegs.head, x8, used_TempRegs.head  
      // MUL  unused_TempRegs.head, unused_TempRegs.head, used_TempRegs.head   
      // SUB  x8, x8, unused_TempRegs.head
      instructions.append(I_UDiv(unused_TempRegs.head, x8, used_TempRegs.head  ))
      instructions.append(I_Mul(unused_TempRegs.head, unused_TempRegs.head, used_TempRegs.head))
      instructions.append(I_Sub(x8, x8, unused_TempRegs.head))


    case LessThan(expr1, expr2) =>
      generateFirst(expr1)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, LT))
      //push and pop x8
        
    case LessThanEq(expr1, expr2) =>
      generateFirst(expr1)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, LE))

    case GreaterThan(expr1, expr2) =>
      generateFirst(expr1)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GT))

    case GreaterThanEq(expr1, expr2) =>
      generateFirst(expr1)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GE))

    case Eq(expr1, expr2) =>
      generateFirst(expr1)
      generateInstructions(expr2)

      instructions.append(I_Cmp(used_TempRegs.head, x8))
      instructions.append(I_CSet(x8, GE))
    
    case NotEq(expr1, expr2) =>
      generateFirst(expr1)
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

  //  // -------------------------- Generate instructions for statements
  //   case Read(lvalue) =>
        
  //   case NewAssignment(identType, name, value) => 
  //     identMap(name) = identMapEntry(getSize(value),0 /*current pointer*/)
  //     value match {
  //       case ArrLiter(e, es) => 
  //         val ess = e+:es
  //         for (expr <- ess) {
  //           generateInstructions(e, unusedRegs, usedRegs)
  //           instructions.append(I_Store(usedRegs.head, sp))
  //           instructions.append(I_Add(sp, sp, ImmVal(getSize(expr))))
  //         }
        
  //       case NewPairRValue(exprL, exprR) =>
  //         val size = getSize(exprL) + getSize(exprR)
  //         instructions.append(I_Move(x0 /* should be head of  */, ImmVal(size)))
  //         instructions.append(I_BranchLink("_malloc"))
  //         instructions.append(I_Move(x16 /* should be implemented to temp reg head*/, x0))


  //         generateInstructions(exprL, unusedRegs, usedRegs)
  //         instructions.append(I_Move(x8, unusedRegs.head))
  //         instructions.append(I_Store(x8, Content(x16/* should be implemented to temp reg head*/, ImmVal(0))))
          

  //         generateInstructions(exprR, unusedRegs, usedRegs)
  //         instructions.append(I_Move(x8, unusedRegs.head))
  //         instructions.append(I_Store(x8, Content(x16/* should be implemented to temp reg head*/, ImmVal(getSize(exprL)))))

  //         instructions.append(I_Move(x8, x16 /* should be implemented to temp reg head*/))
  //         instructions.append(I_Move(unusedRegs.head, x8))
         
          
      //   // add .L.str._ in the front of assembly code 
      //   case StringLiter(value) =>
      //     instructions.append(I_ADRP(x8, s".L.str.$stringCounter"))
      //     stringCounter = stringCounter + 1  
          
      //   case _ => 
      //     generateInstructions(value)
      //     instructions.append(I_Move(unused_ResultRegs.head, x8))
      //     used_ResultRegs = unused_ResultRegs.head +: used_ResultRegs 
      //     unused_ResultRegs.remove(0)
      // }
      

    case Assignment(name, value) => 
    
    case Free(expr) =>
    case Return(expr) =>

    case Exit(expr) =>
    case Print(expr, newline) =>
    case If(condition, thenStat, elseStat) =>
    case While(condition, stat) =>

    case Begin(stmt) =>
    
    case SeqStmt(first, second) => 
      generateInstructions(first)
      generateInstructions(second)
        
    case Skip =>
    

    case IntLiter(value)=> instructions.append(I_Move(x8, ImmVal(value)))
         
    case BoolLiter(value) => 

    case CharLiter(value) => instructions.append(I_Move(x8, ImmValChar(value)))

    case StringLiter(value) =>
    
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


  private def getInstructions: List[Instruction] = instructions.toList


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
    }
  }


  /* ------------------- Helper functions ------------------- */


  private def pushRegs(reg1: Register, reg2: Register):Unit = {
    instructions.append(I_LoadPair(reg1, reg2, Content(sp, ImmVal(0)), ImmVal(16)))
  }

  private def popRegs(reg1: Register, reg2: Register):Unit = {
    instructions.append(I_StorePair(reg1, reg2, Content(sp, ImmVal(0)), ImmVal(16)))
  }


  /*  
    1. generate first expression
    2. store the result in x8
    3. move x8 to unused_ResultRegs.head (x9) 
  */
  def generateFirst(expr1: Expr): Unit = {
    generateInstructions(expr1)
    instructions.append(I_Move(unused_TempRegs.head, x8)) 
  }

}
