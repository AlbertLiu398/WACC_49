package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection.mutable.ListBuffer

object CodeGenerator {

  private val instructions: ListBuffer[Instruction] = ListBuffer()
  private val register_unused : ListBuffer[Register] = ListBuffer() 
  private val register_used: ListBuffer[Register] = ListBuffer()

  def generateInstructions(ast: ASTNode, reg: Register): Unit = ast match {

    // --------------------------  Generate instructions for expressions
    case Add(expr1, expr2) =>
        generateInstructions(expr1, reg)
        generateInstructions(expr2, reg)
        // instructions.append(I_Add())
    
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
        generateInstructions(stmt)
    case SeqStmt(first, second) => 
        generateInstructions(first)
        generateInstructions(second)
    case Skip =>
   
    

    
    case _ => 
  }


  def getInstructions: List[Instruction] = instructions.toList
  
}
