package wacc
object ast{
    sealed trait ASTNode

    sealed trait Type extends PairElemType {
        var getType:String
    }
    case class BaseType(name: String) extends Type with PairElemType{
        var getType: String = name
    }
    case class ArrayType(elementType: Type) extends Type with PairElemType{
        var getType: String =  elementType.getType + "[]"
    }
    case class PairType(first: PairElemType, second: PairElemType) extends Type{
        var getType: String =  s"pair(${first.getType},${second.getType})"
        var getFst: String = first.getType
        var getSnd: String = second.getType
    }

    sealed trait PairElemType extends ASTNode{
        def getType:String
    }
    case object PairTypeElem extends PairElemType{
        override def getType: String =  "pair"
    }

    sealed trait Expr extends ASTNode with RValue {
        def getType: String
    }
    // --------- Binary and Unary Operations ---------
    sealed trait UnaryOperation extends Expr { //
        var getType: String = ""
    }
    sealed trait BinaryOperation extends Expr { //
        var getType: String = ""
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
    
    
    case class ArrLiter(e: Expr, es: List[Expr]) extends Expr with RValue { //
        var getType: String = ""
    }
    case class ArrElem(name: Ident, value: List[Expr]) extends Expr with LValue { //
        var getType: String = ""
    }
    
    sealed trait UnaryOperator extends ASTNode
    case class UOper(name: String) extends UnaryOperator

    sealed trait BinaryOperator extends ASTNode
    case class BOper(name: String) extends BinaryOperator

    sealed trait LValue extends ASTNode {
        var getType: String
    }
    
    sealed trait PairElem extends LValue with RValue
    case class FstPairElem(values: LValue) extends PairElem{
        var getType: String = ""
    }
    case class SndPairElem(values: LValue) extends PairElem{
        var getType: String = ""
    }

    sealed trait RValue extends ASTNode {
        var getType: String
        var getFst: String = ""
        var getSnd: String = ""
    }
    case class NewPairRValue(exprL: Expr, exprR: Expr) extends RValue { //
        var getType: String = ""
    }
    case class CallRValue(func: Ident, args: ArgList) extends RValue { //
        var getType: String = ""
    }
    
    sealed trait Stmt extends ASTNode
    case object Skip extends Stmt
    case class NewAssignment(identType: Type, name: Ident, value: RValue) extends Stmt //
    case class Assignment(lvalue: LValue, rvalue: RValue) extends Stmt //
    case class Read(lvalue: LValue) extends Stmt //
    case class Free(expr: Expr) extends Stmt //
    case class Return(expr: Expr) extends Stmt //
    case class Exit(expr: Expr) extends Stmt 
    case class Print(expr: Expr, newline: Boolean) extends Stmt //
    case class If(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt //
    case class While(condition: Expr, body: Stmt) extends Stmt //
    case class Begin(stmt: Stmt) extends Stmt //
    case class SeqStmt(first: Stmt, second: Stmt) extends Stmt //

    sealed trait Liter extends Expr {
        var getType: String
    }
    case class IntLiter(value: BigInt) extends Liter {
        var getType = "int"
    }
    case class BoolLiter(value: Boolean) extends Liter {
        var getType = "bool" 
    }
    case class CharLiter(value: Char) extends Liter {
        var getType = "char" 
    }
    case class StringLiter(value: String) extends Liter {
        var getType = "string" 
    }
    case object PairLiter extends Liter {
        var getType = "pair" 
    }

    case class Ident(value: String) extends Expr with LValue {
        var getType: String = ""
    }
    case class Param(paramType: Type, paramName: Ident) extends ASTNode {
        val getType: String = paramType.getType
    }
    case class ParamList(paramListType: List[Param]) extends ASTNode {
        val getType: List[String] = paramListType.map(_.getType)
    }
    case class ArgList(exprl: List[Expr]) extends ASTNode //
    case class Func(returnType: Type, functionName: Ident, params: ParamList, body: Stmt) extends ASTNode //
    case class Program(functions: List[Func], statements: Stmt) extends ASTNode //
}
