package wacc
object ast{
    sealed trait ASTNode

    sealed trait Type extends ASTNode
    case class BaseType(name: String) extends Type
    case class ArrayType(elementType: Type) extends Type
    case class PairType(first: PairElemType, second: PairElemType) extends Type

    sealed trait PairElemType extends ASTNode
    case class BaseTypeElem(name: String) extends PairElemType
    case class ArrayTypeElem(elementType: Type) extends PairElemType
    case object PairTypeElem extends PairElemType

    sealed trait Expr extends ASTNode
    case class UnaryOperation(operator: UnaryOperator, expr: Expr) extends Expr
    case class BinaryOperation(operator: BinaryOperator, left: Expr, right: Expr) extends Expr
    case class Atom(value: String) extends Expr
    
    sealed trait UnaryOperator extends ASTNode
    case class Uopr(name: String) extends UnaryOperator

    sealed trait BinaryOperator extends ASTNode
    case class BOper(name: String) extends BinaryOperator

    sealed trait LValue extends ASTNode
    case class IdentLValue(name: Ident) extends LValue
    case class ArrayElemLValue(ident: String, indices: List[Expr]) extends LValue
    case class PairElemLValue(access: PairElem,lvalue: LValue) extends LValue
    
    sealed trait PairElem extends ASTNode
    case class FstPairElemRValue(value: LValue) extends PairElem
    case class SndPairElemRValue(value: LValue) extends PairElem
    

    sealed trait RValue extends ASTNode
    case class ExprRValue(expr: Expr) extends RValue
    case class NewPairRValue(exprL: Expr, exprR: Expr) extends RValue 
    case class ArrayLiterRValue(expressions: List[Expr]) extends RValue
    case class CallRValue(func: Ident, args: List[Expr]) extends RValue

    sealed trait Stmt extends ASTNode
    case object Skip extends Stmt
    case class NewAssignment(identType: Type, name: String, value: ASTNode) extends Stmt
    case class Assignment(lvalue: LValue, rvalue: ASTNode) extends Stmt
    case class Read(lvalue: LValue) extends Stmt
    case class Free(expr: Expr) extends Stmt
    case class Return(expr: Expr) extends Stmt
    case class Exit(expr: Expr) extends Stmt
    case class Print(expr: Expr, newline: Boolean) extends Stmt
    case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt
    case class While(condition: Expr, body: Stmt) extends Stmt
    case class Begin(stmt: Stmt) extends Stmt
    case class SeqStmt(left: Stmt, right: Stmt) extends Stmt

    sealed trait Liter extends ASTNode
    case class IntLiter(value: BigInt) extends Liter
    case class BoolLiter(value: Boolean) extends Liter
    case class CharLiter(value: Char) extends Liter
    case class StringLiter(value: String) extends Liter
    case object PairLiter extends Liter

    case class arrElem(name: Ident, value: List[Expr]) extends ASTNode
    case class Ident(value: String) extends ASTNode
    case class Param(paramType: Type, paramName: Ident) extends ASTNode
    case class Func(returnType: Type, functionName: Ident, params: List[Param], body: Stmt) extends ASTNode
    case class Program(functions: List[Func], statements: Stmt) extends ASTNode
}

