package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley._
import parsley.syntax._

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
    private lazy val paramList = ???
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
    private lazy val lValue: Parsley[LValue] = IdentLValue.lift(ident) | arr | pairElem
    private lazy val pairElem: Parsley[LValue] = "fst" ~> PairElem.lift("fst", lValue) | "snd" ~> PairElem.lift("snd", lValue)

    private lazy val rValue = 
        ExprRValue.lift(expr) | 
        ArrayLiterRValue.lift(arrLiter) | 
        NewPairRValue.lift("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") |
        FstPairElemRValue.lift("fst" ~> lValue) |
        SndPairElemRValue.lift("snd" ~> lValue) |
        CallRValue.lift("call" ~> ident, "(" ~> argsList <~ ")")
    
    
    // private lazy val argsList: Parsley[ArgList] = ArgList.lift(lexer.commaSep(expr))
    // private lazy val arrLiter = ???
    // private lazy val arrLiterOrExpr

    // -------------------------- Types ---------------------------
    private lazy val allType: Parsley[Type] = baseType | arrayType | pairType

    private lazy val baseType: Parsley[Type] = "int" #> BaseType("int") | "bool" #> BaseType("bool") | "char" #> BaseType("char") | "string" #> BaseType("string")
    private lazy val arrayType: Parsley[Type] = ArrayType.lift(allType <~ "[" <~ "]")
    private lazy val pairType: Parsley[Type] = PairType.lift("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")

    private lazy val pairElemType: Parsley[PairElemType] = baseTypeElem | arrayTypeElem | pairTypeElem
    private lazy val baseTypeElem: Parsley[PairElemType] = "int" #> BaseTypeElem("int") | "bool" #> BaseTypeElem("bool") | "char" #> BaseTypeElem("char") | "string" #> BaseTypeElem("string")
    private lazy val arrayTypeElem: Parsley[PairElemType] = ArrayTypeElem.lift(allType <~ "[" <~ "]")
    private lazy val pairTypeElem: Parsley[PairElemType] = "pair" #> PairTypeElem
    


    // -------------------------- Expressions --------------------------
    private lazy val expr: Parsley[Expr]= UnaryOperation.lift(uOper, expr) | BinaryOperation.lift(bOper, expr, expr) | atom
    private lazy val atom =  intLiter | boolLiter | charLiter | stringLiter | pairLiter | ident | "(" ~> expr <~ ")" | arr
    private lazy val uOper =  "!" #> Uopr("!") | "-"  #> Uopr("-") | "len" #> Uopr("len") |  "ord" #> Uopr("ord")| "chr" #> Uopr("chr")
    private lazy val bOper = "*" #> BOper("*")| "/" #> BOper("/")| "%" #> BOper("%")| "+" #> BOper("+")| 
                            "-" #> BOper("-")| "<" #> BOper("<")| ">" #> BOper(">")| "<=" #> BOper("<=" )| 
                            ">=" #> BOper(">=")| "==" #> BOper("==")| "!=" #> BOper("!=")| "&&" #> BOper("&&")| "||" #> BOper("||")
    private lazy val arr = ArrElem.lift(ident, some("[" ~> expr <~ "]"))



}
