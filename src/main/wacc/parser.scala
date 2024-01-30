package wacc

import parsley.{Parsley, Result}
import parsley.expr._
import parsley.Parsley._
import parsley.syntax._
import scala.language.postfixOps

import lexer.implicits.implicitSymbol
import lexer._
import ast._

object parser {
    import parsley.syntax.lift.{Lift1, Lift2, Lift3, Lift4}
    def parse(input: String) = parser.parse(input)
    private val parser = fully(prog)

    // -------------------------- Literals -------------------------
    private lazy val intLiter = integer.map(IntLiter)
    private lazy val ident = identifier.map(Ident)
    private lazy val boolLiter = ("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(false))
    private lazy val charLiter = graphicCharacter.map(CharLiter)
    // private lazy val charLiter = between(char('\''), "'", character.map(CharLiter))
    private lazy val stringLiter = string.map(StringLiter)
    private lazy val pairLiter = "null" #> PairLiter


    // TODO : All implicits "x" may need to be replaced with lexer.keyword("x")
    // TODO : Implicits parenthesis "(" ~> x <~ ")" may be replaced with lexer.parens(x)
    // TODO : Statements may need more than one parsers

    // -------------------------- Statements -------------------------
    private lazy val prog: Parsley[Program] = Program.lift("begin" ~> many(func) , stmt <~ "end")
    private lazy val func: Parsley[Func] = Func.lift(allType, ident, "(" ~> paramList <~ ")", "is" ~> stmt <~ "end")
    private lazy val paramList: Parsley[ParamList] = ParamList.lift(commaSep_(param))
    private lazy val param = Param.lift(allType, ident)
    private lazy val stmt: Parsley[Stmt] = 
        SeqStmt.lift(stmt <~ ";", stmt) |
        "skip" #> Skip |
        NewAssignment.lift(allType, ident, "=" ~> rValue) |
        Assignment.lift(lValue, "=" ~> rValue) |
        "read" ~> Read.lift(lValue) |
        "free" ~> Free.lift(expr) |
        "return" ~> Return.lift(expr) |
        "exit" ~> Exit.lift(expr) |
        "print" ~> Print.lift(expr, pure(false)) |
        "println" ~> Print.lift(expr, pure(true)) |
        If.lift("if" ~> expr, "then" ~> stmt, "else" ~> stmt) |
        While.lift("while" ~> expr, "do" ~> stmt <~ "done") |
        Begin.lift("begin" ~> stmt <~ "end")
    private lazy val arrl: Parsley[ArrElemLValue] = ArrElemLValue.lift(ident, some("[" ~> expr <~ "]"))
    private lazy val lValue: Parsley[LValue] = IdentLValue.lift(ident) | arrl | pairElem
    private lazy val pairElem = "fst" ~> PairElem.lift(pure("fst"), lValue) | "snd" ~> PairElem.lift(pure("snd"), lValue)

    private lazy val rValue = 
        ExprRValue.lift(expr) | 
        ArrayLiterRValue.lift(arrLiter) | 
        NewPairRValue.lift("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") |
        pairElem |
        CallRValue.lift("call" ~> ident, "(" ~> argsList <~ ")")
    
    private lazy val argsList: Parsley[ArgList] = ArgList.lift(commaSep_(exprOrArrayLit))
    private lazy val arrLiter: Parsley[ArrLiter] = "[]" #> ArrLiter(null, List()) <|> ArrLiter.lift("[" ~> exprOrArrayLit, commaSep_(exprOrArrayLit) <~ "]")
    private lazy val exprOrArrayLit: Parsley[Expr] = expr | arrLiter

    // -------------------------- Types ---------------------------
    private lazy val allType: Parsley[Type] = baseType | arrayType | pairType
    private lazy val notArrayType: Parsley[Type] = baseType | pairType
    private lazy val baseType: Parsley[Type] = "int" ~> BaseType.lift(pure("int")) | "bool" ~> BaseType.lift(pure("bool")) | "char" ~> BaseType.lift(pure("char")) | "string" ~> BaseType.lift(pure("string"))
    private lazy val arrayType: Parsley[Type] = chain.postfix(notArrayType)("[]".as(ArrayType))

    private lazy val pairType: Parsley[Type] = PairType.lift("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")

    private lazy val pairElemType: Parsley[PairElemType] = baseTypeElem | arrayTypeElem | pairTypeElem
    private lazy val baseTypeElem: Parsley[PairElemType] = "int" #> BaseTypeElem("int") | "bool" #> BaseTypeElem("bool") | "char" #> BaseTypeElem("char") | "string" #> BaseTypeElem("string")
    private lazy val arrayTypeElem: Parsley[PairElemType] = ArrayTypeElem.lift(allType <~ "[" <~ "]")
    private lazy val pairTypeElem: Parsley[PairElemType] = "pair" #> PairTypeElem
    


    // -------------------------- Expressions --------------------------
    private lazy val expr: Parsley[Expr]= UnaryOperation.lift(uOper, expr) | BinaryOperation.lift(bOper, expr, expr) | atom
    private lazy val atom : Parsley[Expr] =  intLiter | boolLiter | charLiter | stringLiter | pairLiter | ident | arr
    private lazy val uOper: Parsley[UOper]  =  "!" #> UOper("!") | "-"  #> UOper("-") | "len" #> UOper("len") |  "ord" #> UOper("ord")| "chr" #> UOper("chr")
    private lazy val bOper: Parsley[BOper] = "*" #> BOper("*")| "/" #> BOper("/")| "%" #> BOper("%")| "+" #> BOper("+")| 
                            "-" #> BOper("-")| "<" #> BOper("<")| ">" #> BOper(">")| "<=" #> BOper("<=" )| 
                            ">=" #> BOper(">=")| "==" #> BOper("==")| "!=" #> BOper("!=")| "&&" #> BOper("&&")| "||" #> BOper("||")
    private lazy val arr: Parsley[ArrElem] = ArrElem.lift(ident, some("[" ~> expr <~ "]"))
}
