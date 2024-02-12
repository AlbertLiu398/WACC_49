package wacc

import wacc.ast._
import wacc.instruction._
import scala.collection.mutable.ListBuffer

object CodeGenerator {

  private val instructions: ListBuffer[Instruction] = ListBuffer()
  private val register_unused : ListBuffer[Register] = ListBuffer() 
  private val register_used: ListBuffer[Register] = ListBuffer()

  def generateInstructions(ast: ASTNode, regsInUse: Register, unusedRegs : ListBuffer[Register],
                           usedRegs : ListBuffer[Register]): Unit = ast match {

    // --------------------------  Generate instructions for expressions
    case Add(expr1, expr2) =>
        val (reg1, unusedRegs1) = getUnusedRegister(unusedRegs)
        val (reg2, unusedRegs2) = getUnusedRegister(unusedRegs1)

        val updatedUsedRegs = usedRegs ++ List(reg1, reg2)
        generateInstructions(expr1, reg1, unusedRegs2, updatedUsedRegs)
        generateInstructions(expr2, reg2, unusedRegs2, updatedUsedRegs)
        // instructions.append(I_Add(

    
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
        
    case Skip =>
   
    

    
    case _ => 
  }


  private def getInstructions: List[Instruction] = instructions.toList

  private def getUnusedRegister(unusedRegs: ListBuffer[Register]): (Register, ListBuffer[Register]) = {
    val reg = unusedRegs.head  // represents the first register that is currently available to use 
    (reg, unusedRegs.tail)     // returns the first register and the rest of the unused_registers
  }
  
}
