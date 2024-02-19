package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection._
import conditions._
import shift._
import Constant._
object CodeGenerator {

  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()
  private val variableMap: Map[String, Int] = Map()

  def generateInstructions(ast: ASTNode, unusedRegs: mutable.ListBuffer[Register],
                           usedRegs: mutable.ListBuffer[Register]): Unit = ast match {

    case Program(functions, statements) => 
      


    case Func(returnType, functionName, params, body) => 

    // --------------------------  Generate instructions for expressions
    case Add(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Add(usedRegs.head, usedRegs.head, unusedRegs.head))
    
    case Sub(expr1, expr2) => 
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Sub(usedRegs.head, usedRegs.head, unusedRegs.head))

    case Mul(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Mul(usedRegs.head, usedRegs.head, unusedRegs.head))

    case Div(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_UDiv(usedRegs.head, usedRegs.head, unusedRegs.head))

    case Mod(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      // UDIV x2, x0, x1    
      // MUL  x2, x2, x1   
      // SUB  x0, x0, x2  
      instructions.append(I_UDiv(unusedRegs.tail.head, usedRegs.head, unusedRegs.head))
      instructions.append(I_Mul(unusedRegs.tail.head, unusedRegs.tail.head, unusedRegs.head))
      instructions.append(I_Sub(usedRegs.head, usedRegs.head, unusedRegs.tail.head))

    case LessThan(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Cmp(usedRegs.head, unusedRegs.head))
      instructions.append(I_CSet(usedRegs.head, LT))
        
    case LessThanEq(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Cmp(usedRegs.head, unusedRegs.head))
      instructions.append(I_CSet(usedRegs.head, LE))

    case GreaterThan(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Cmp(usedRegs.head, unusedRegs.head))
      instructions.append(I_CSet(usedRegs.head, GT))

    case GreaterThanEq(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Cmp(usedRegs.head, unusedRegs.head))
      instructions.append(I_CSet(usedRegs.head, GE))

    case Eq(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Cmp(usedRegs.head, unusedRegs.head))
      instructions.append(I_CSet(usedRegs.head, EQ))
    
    case NotEq(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Cmp(usedRegs.head, unusedRegs.head))
      instructions.append(I_CSet(usedRegs.head, NE))

    case And(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_And(usedRegs.head, usedRegs.head, unusedRegs.head))

    case Or(expr1, expr2) =>
      binary(unusedRegs, usedRegs, expr1, expr2)
      instructions.append(I_Orr(usedRegs.head, usedRegs.head, unusedRegs.head))

    case Invert(expr) =>
      generateInstructions(expr, unusedRegs, usedRegs) 
      instructions.append(I_Xor(usedRegs.head, usedRegs.head, ImmVal(1)))

    case Negate(expr) =>
      generateInstructions(expr, unusedRegs, usedRegs)
      instructions.append(I_Neg(usedRegs.head, usedRegs.head, LSL(0)))

    case Len(expr) =>
      generateInstructions(expr, unusedRegs, usedRegs)

    case Ord(expr) =>
      generateInstructions(expr, unusedRegs, usedRegs)
      instructions.append(I_Sub(usedRegs.head, usedRegs.head, ImmVal(0)))


    case Chr(expr) =>   
      generateInstructions(expr, unusedRegs, usedRegs)
      instructions.append(I_Add(usedRegs.head, usedRegs.head, ImmValChar(0)))

   // -------------------------- Generate instructions for statements
    case Read(lvalue) =>
        
    case NewAssignment(identType, name, value) => 
      
      value match {
        case ArrLiter(e, es) => 
          val ess = e+:es
          for (expr <- ess) {
            generateInstructions(e, unusedRegs, usedRegs)
            instructions.append(I_Store(usedRegs.head, x31))
            instructions.append(I_Add(x31, x31, ImmVal(1)/* should be size of value */))
          }
        
        case NewPairRValue(exprL, exprR) => 


        case _ => 
          generateInstructions(value, unusedRegs, usedRegs)
          instructions.append(I_Store(usedRegs.head, x31))
          instructions.append(I_Add(x31, x31, ImmVal(1)/* should be size of value */))
      }
      
    case Assignment(name, value) => 
    
    case Free(expr) =>
    case Return(expr) =>

    case Exit(expr) =>
    case Print(expr, newline) =>
    case If(condition, thenStat, elseStat) =>
    case While(condition, stat) =>

    case Begin(stmt) =>
    
    case SeqStmt(first, second) => 
      generateInstructions(first, unusedRegs, usedRegs)
      generateInstructions(second, unusedRegs.tail, usedRegs :+ unusedRegs.head)
        
    case Skip =>

   
    

    
    case _ => 
  }


  private def getInstructions: List[Instruction] = instructions.toList

  // private def getUnusedRegister(unusedRegs: mutable.ListBuffer[Register]): (Register, mutable.ListBuffer[Register]) = {

  //   val reg = unusedRegs.head  // represents the first register that is currently available to use )     // returns the first register and the rest of the unused_registers
  //   val unusedRegs1 = unusedRegs.tail
  //   val unusedRegs2 = getUnusedRegister(unusedRegs1)

  //   val updatedUsedRegs = usedRegs ++ List(reg1)
  //   generateInstructions(expr1, reg1, unusedRegs2, updatedUsedRegs)
  //   generateInstructions(expr2, reg2, unusedRegs2, updatedUsedRegs)
    
  // }
  private def binary(usedRegs:mutable.ListBuffer[Register] ,unusedRegs: mutable.ListBuffer[Register], expr1: Expr, expr2: Expr): Unit = {
      
      val reg1 = unusedRegs.head
      val unusedRegs1: mutable.ListBuffer[Register] = unusedRegs.tail

      val updatedUsedRegs = usedRegs ++ List(reg1)
      generateInstructions(expr1, unusedRegs,usedRegs)
      generateInstructions(expr2, unusedRegs1, updatedUsedRegs)
  }
}
