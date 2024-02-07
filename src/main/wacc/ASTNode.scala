package wacc
object ast{
    sealed trait ASTNode

    sealed trait Type extends PairElemType
    case class BaseType(name: String) extends Type with PairElemType
    case class ArrayType(elementType: Type) extends Type with PairElemType
    case class PairType(first: PairElemType, second: PairElemType) extends Type

    sealed trait PairElemType extends ASTNode
    case object PairTypeElem extends PairElemType 

    sealed trait Expr extends ASTNode with RValue {
        def getType: String
    }
    // --------- Binary and Unary Operations ---------
    sealed trait UnaryOperation extends Expr {
        val getType: String = ""
    }
    sealed trait BinaryOperation extends Expr {
        val getType: String = ""
    }

    case class Add(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class Sub(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class Mul(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class Div(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class Mod(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class LessThan(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class LessThanEq(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class GreaterThan(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class GreaterThanEq(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class Eq(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class NotEq(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class And(expr1: Expr, expr2: Expr) extends BinaryOperation
    case class Or(expr1: Expr, expr2: Expr) extends BinaryOperation

    case class Invert(expr: Expr) extends UnaryOperation
    case class Negate(expr: Expr) extends UnaryOperation
    case class Len(expr: Expr) extends UnaryOperation
    case class Ord(expr: Expr) extends UnaryOperation
    case class Chr(expr: Expr) extends UnaryOperation
    
    
    case class ArrLiter(e: Expr, es: List[Expr]) extends Expr with RValue {
        val getType: String = ""
    }
    case class ArrElem(name: Ident, value: List[Expr]) extends Expr with LValue {
        val getType: String = ""
    }
    
    sealed trait UnaryOperator extends ASTNode
    case class UOper(name: String) extends UnaryOperator

    sealed trait BinaryOperator extends ASTNode
    case class BOper(name: String) extends BinaryOperator

    sealed trait LValue extends ASTNode
    
    sealed trait PairElem extends LValue with RValue
    case class FstPairElem(values: LValue) extends PairElem 
    case class SndPairElem(values: LValue) extends PairElem

    sealed trait RValue extends ASTNode
    case class NewPairRValue(exprL: Expr, exprR: Expr) extends RValue 
    case class CallRValue(func: Ident, args: ArgList) extends RValue
    
    sealed trait Stmt extends ASTNode
    case object Skip extends Stmt
    case class NewAssignment(identType: Type, name: Ident, value: ASTNode) extends Stmt
    case class Assignment(lvalue: LValue, rvalue: ASTNode) extends Stmt
    case class Read(lvalue: LValue) extends Stmt
    case class Free(expr: Expr) extends Stmt
    case class Return(expr: Expr) extends Stmt
    case class Exit(expr: Expr) extends Stmt
    case class Print(expr: Expr, newline: Boolean) extends Stmt
    case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt
    case class While(condition: Expr, body: Stmt) extends Stmt
    case class Begin(stmt: Stmt) extends Stmt
    case class SeqStmt(first: Stmt, second: Stmt) extends Stmt

    sealed trait Liter extends Expr
    case class IntLiter(value: BigInt) extends Liter {
        val getType = "int"
    }
    case class BoolLiter(value: Boolean) extends Liter {
        val getType = "bool" 
    }
    case class CharLiter(value: Char) extends Liter {
        val getType = "char" 
    }
    case class StringLiter(value: String) extends Liter {
        val getType = "string" 
    }
    case object PairLiter extends Liter {
        val getType = "pair" 
    }

    case class Ident(value: String) extends Expr with LValue {
        val getType: String = ""
    }
    case class Param(paramType: Type, paramName: Ident) extends ASTNode
    case class ParamList(paramListType: List[Param]) extends ASTNode
    case class ArgList(exprl: List[Expr]) extends ASTNode
    case class Func(returnType: Type, functionName: Ident, params: ParamList, body: Stmt) extends ASTNode
    case class Program(functions: List[Func], statements: Stmt) extends ASTNode
}
