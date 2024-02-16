package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection._

object CodeGenerator {

  private val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer()

  def generateInstructions(ast: ASTNode, unusedRegs: mutable.ListBuffer[Register],
                           usedRegs: mutable.ListBuffer[Register]): Unit = ast match {

    // --------------------------  Generate instructions for expressions
    case Add(expr1, expr2) =>
      unary(unusedRegs, usedRegs, expr1, expr2)
      
      instructions.append(I_Add(usedRegs(0), unusedRegs(0), usedRegs(2)))

    
    // case Sub(expr1, expr2) => 
    //     generateInstructions(expr1)
    //     generateInstructions(expr2)


    // case Mul(expr1, expr2) =>
    //     generateInstructions(expr1)
    //     generateInstructions(expr2)

    // case Div(expr1, expr2) =>
    //     generateInstructions(expr1)
    //     generateInstructions(expr2)

    // case Mod(expr1, expr2) =>
    //     generateInstructions(expr1)
    //     generateInstructions(expr2)

    // case LessThan(expr1, expr2) =>
    //     generateInstructions(expr1)
    //     generateInstructions(expr2)
        
    case LessThanEq(expr1, expr2) =>

    case GreaterThan(expr1, expr2) =>

    case GreaterThanEq(expr1, expr2) =>

    case Eq(expr1, expr2) =>
    
    case NotEq(expr1, expr2) =>

    case And(expr1, expr2) =>

    case Or(expr1, expr2) =>

    case Invert(expr) =>

    case Negate(expr) =>

    case Len(expr) =>

    case Ord(expr) =>

    case Chr(expr) =>   

   // -------------------------- Generate instructions for statements

    case Skip => 

    case Read(lvalue) =>
        
    case NewAssignment(identType, name, value) => 
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
  private def unary(usedRegs:mutable.ListBuffer[Register] ,unusedRegs: mutable.ListBuffer[Register], expr1: Expr, expr2: Expr): Unit = {
      
      val reg1 = unusedRegs.head
      val unusedRegs1: mutable.ListBuffer[Register] = unusedRegs.tail

      val updatedUsedRegs = usedRegs ++ List(reg1)
      generateInstructions(expr1, unusedRegs, updatedUsedRegs)
      generateInstructions(expr2, unusedRegs1, updatedUsedRegs)
  }
}
