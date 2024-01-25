package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley._
import parsley.syntax._

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}
import _empty_.Skip
import _empty_.CallRValue

object parser {
    
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private val parser = fully(prog)

    // TODO? : All implicits "x" may need to be replaced with lexer.keyword("x")
    // TODO? : Implicits parenthesis "(" ~> x <~ ")" may be replaced with lexer.parens(x)
    // TODO? : Statements may need more than one parsers

    // -------------------------- Statements -------------------------
    private lazy val prog = Program.lift("begin" ~> many(func) , stmt <~ "end")
    private lazy val func = Func.lift(allType, ident, "(" ~> paramList <~ ")", "is" ~> stmt <~ "end")
    private lazy val paramList = ???
    private lazy val param = Param.lift(allType, ident)
    private lazy val stmt = 
        SeqStmt.lift(stmt <~ ";", stmt) |
        "skip" ~> Skip |
        newAssignment.lift(allType, ident, "=" ~> rValue) |
        Assignment.lift(lValue, "=" ~> rValue) |
        "read" ~> Read.lift(lValue) |
        "free" ~> Free.lift(expr) |
        "return" ~> Return.lift(expr) |
        "exit" ~> Exit.lift(expr) |
        "print" ~> Print.lift(expr, false) |
        "println" ~> Print.lift(expr, true) |
        If.lift("if" ~> expr, "then" ~> stmt, "else" ~> stmt) |
        While.lift("while" ~> expr, "do" ~> stmt <~ "done") |
        Begin.lift("begin" ~> stmt <~ "end")
    private lazy val lValue = IdentLValue.lift(ident) | ArrayElemLValue.lift(arrElem) | PairElemLValue.lift(pairElem)
    private lazy val rValue = 
        ExprRValue.lift(expr) | 
        ArrayLiterRValue.lift(arrLiter) | 
        NewPairRValue.lift("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") |
        FstPairElemRValue.lift("fst" ~> lValue) |
        SndPairElemRValue.lift("snd" ~> lValue) |
        CallRValue.lift("call" ~> ident, "(" ~> argsList <~ ")")
    private lazy val argsList = ???
    private lazy val arrLiter = ???

    // -------------------------- Types ---------------------------
    private lazy val allType = baseType | arrayType | pairType
    private lazy val baseType = "int" | "bool" | "char" | "string"
    private lazy val arrayType = ArrayType.lift(allType ~> '[' ~> ']')
    private lazy val pairType = PairType.lift("pair" ~> '(' pairElemType, ',' ~> pairElemType <~ ')')

    private lazy val pairElemType = baseTypeElem | arrayTypeElem | "pair"
    private lazy val baseTypeElem = baseType
    private lazy val arrayTypeElem = arrayType

    





    // -------------------------- Expressions --------------------------
    private lazy val expr = Expr.lift(uOper)
    private lazy val atom =
    private lazy val uOper =   
    private lazy val bOper =
    private lazy val arrElem =


    


   

    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] =
        chain.left1(integer | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
}
