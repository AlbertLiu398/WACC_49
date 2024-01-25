package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.Parsley._
import parsley.syntax._

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}

object parser {
    
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private val parser = fully(prog)

    // TODO? : All implicits "x" may need to be replaced with lexer.keyword("x")
    // TODO? : Implicits parenthesis "(" ~> x <~ ")" may be replaced with lexer.parens(x)

    // -------------------------- Statements -------------------------
    private lazy val prog = Program.lift("begin" ~> many(func) , stmt <~ "end")
    private lazy val func = Func.lift(allType, ident, "(" ~> paramList <~ ")", "is" ~> stmt <~ "end")
    private lazy val paramList = ???
    private lazy val param = Param.lift(allType, ident)
    private lazy val stmt = "skip" | Assignment.lift(allType, ident, "=" ~> rValue)


    // -------------------------- Types ----------- ---------------
    private lazy val allType = BaseType.lift(baseType) | ArrayType.lift(arrayType) | PairType.lift(pairType)
    private lazy val baseType = lexer."int" | "bool" | "char" | "string"
    private lazy val arrayType = 
    private lazy val pairType

    





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
