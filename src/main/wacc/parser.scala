package wacc

import parsley.{Parsley, Result}
import parsley.expr._
import parsley.Parsley._
import parsley.syntax._
import scala.language.postfixOps
import parsley.debug, debug._ 

import lexer.implicits.implicitSymbol
import lexer._
import ast._
import parsley.character.noneOf

object parser {
    import parsley.syntax.lift.{Lift1, Lift2, Lift3, Lift4}

    def parse(input: String) = parser.parse(input)
    def funcParse(input: String) = funcParser.parse(input)
    def paramListParse(input: String) = paramListParser.parse(input)
    def paramParse(input: String) = paramParser.parse(input)
    def stmtParse(input: String) = stmtParser.parse(input)
    def lValueParse(input: String) = lValueParser.parse(input)
    def rValueParse(input: String) = rValueParser.parse(input)
    def argsListParse(input: String) = argsListParser.parse(input)
    def arrLiterParse(input: String) = arrLiterParser.parse(input)
    def pairElemParse(input: String) = pairElemParser.parse(input)
    def arrlParse(input: String) = arrlParser.parse(input)
    def allTypeParse(input: String) = allTypeParser.parse(input)
    def exprParse(input: String) = exprParser.parse(input)

    
    // -------------- Parser --------------
    private val parser = fully(prog)
    private val funcParser = fully(func)
    private val paramListParser = fully(paramList)
    private val paramParser = fully(param)
    private val stmtParser = fully(stmt)
    private val lValueParser = fully(lValue)
    private val rValueParser = fully(rValue)
    private val argsListParser = fully(argsList)
    private val arrLiterParser = fully(arrLiter)
    private val pairElemParser = fully(pairElem)
    private val arrlParser = fully(arr)
    private val allTypeParser = fully(allType)
    private val exprParser = fully(expr)
//    //-------------------comment ------------
//    private lazy val comment: Parsley[Unit] = "#" ~> many(noneOf("\n".toSet)) ~> "\n" ~> pure(())

    // -------------------------- Literals -------------------------
    private lazy val intLiter = integer.map(IntLiter)
    private lazy val ident = identifier.map(Ident)
    private lazy val boolLiter = ("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(false))
    private lazy val charLiter =  graphicCharacter.map(CharLiter)
    // private lazy val charLiter = between(char('\''), "'", character.map(CharLiter))
    private lazy val stringLiter = string.map(StringLiter)
    private lazy val pairLiter = "null" #> PairLiter


    // TODO : All implicits "x" may need to be replaced with lexer.keyword("x")
    // TODO : Implicits parenthesis "(" ~> x <~ ")" may be replaced with lexer.parens(x)
    // TODO : Statements may need more than one parsers

    // -------------------------- Statements -------------------------
    private lazy val prog: Parsley[Program] = Program.lift("begin" ~> many(func), stmt <~ "end")
    private lazy val func: Parsley[Func] = atomic(Func.lift(allType, ident, paramList, "is" ~> stmt <~ "end"))
    private lazy val paramList: Parsley[ParamList] = "(" ~> ParamList.lift(commaSep1_(param)) <~ ")"
    private lazy val param = Param.lift(allType, ident)
    private lazy val stmtAtom: Parsley[Stmt] = 
        "skip" #> Skip|
        NewAssignment.lift(allType, ident, "=" ~> rValue) |
        Assignment.lift(lValue, "=" ~> rValue) |
        "read" ~> Read.lift(lValue) |
        "free" ~> Free.lift(expr) |
        "return" ~> Return.lift(expr) |
        "exit" ~> Exit.lift(expr) |
        "print" ~> Print.lift(expr, pure(false)) |
        "println" ~> Print.lift(expr, pure(true)) |
        If.lift("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi") |
        While.lift("while" ~> expr, "do" ~> stmt <~ "done") |
        Begin.lift("begin" ~> stmt <~ "end")
    private lazy val stmt =  atomic(stmtAtom <~ notFollowedBy(";")) | stmtJoin
    private lazy val stmtJoin: Parsley[Stmt] = SeqStmt.lift(stmtAtom <~ ";", stmt)
    //using parser bridge and option to avoid amubiguity
    private lazy val lValue: Parsley[LValue] = pairElem | arr | atomic(ident <~ notFollowedBy("["))

    private lazy val notPairElem: Parsley[LValue] = atomic(ident <~ notFollowedBy("[")) | arr
    private lazy val pairElem = fstPairElem | sndPairElem
    private lazy val fstPairElem = chain.prefix(notPairElem)("fst".as(FstPairElem))
    private lazy val sndPairElem = chain.prefix(notPairElem)("snd".as(SndPairElem))

    
    private lazy val rValue =
        NewPairRValue.lift("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") |
        CallRValue.lift("call" ~> ident, "(" ~> argsList <~ ")") |
        expr |
        pairElem | 
        arrLiter 
      
    private lazy val argsList: Parsley[ArgList] = ArgList.lift(commaSep1_(exprOrArrayLit))
    private lazy val arrLiter: Parsley[ArrLiter] = "[]" #> ArrLiter(null, List()) | ArrLiter.lift("[" ~> expr <~ ",", commaSep_(expr) <~ "]") | atomic(ArrLiter.lift("[" ~> expr, pure(List()) <~ "]"))
    private lazy val exprOrArrayLit: Parsley[Expr] = expr | arrLiter

    // -------------------------- Types ---------------------------
    private lazy val allType: Parsley[Type] = arrayType | notArrayType
    private lazy val notArrayType: Parsley[Type] = baseType | pairType
    private lazy val baseType = "int" ~> BaseType.lift(pure("int")) | "bool" ~> BaseType.lift(pure("bool")) | "char" ~> BaseType.lift(pure("char")) | "string" ~> BaseType.lift(pure("string"))
    private lazy val arrayType = chain.postfix(notArrayType)("[]".as(ArrayType))
    private lazy val pairType: Parsley[Type] = "pair" ~> "(" ~> PairType.lift(pairElemType, "," ~> pairElemType) <~ ")"

    private lazy val pairElemType: Parsley[PairElemType] = arrayType | baseType | pairTypeElem
    private lazy val pairTypeElem: Parsley[PairElemType] = "pair" #> PairTypeElem

    // -------------------------- Expressions --------------------------
    
    private lazy val expr: Parsley[Expr]= operators | atom
    // private lazy val a = operators
    // private lazy val bExpr : Parsley[Expr] = chain.left1(notUOper)("+".as(Add) | "-".as(Sub) | "*".as(Mul) | "/".as(Div) | "%".as(Mod) 
    // | "<".as(LessThan) | ">".as(GreaterThan) | "<=".as(LessThanEq) | ">=".as(GreaterThanEq) | "==".as(Eq) | "!=".as(NotEq) | "&&".as(And) | "||".as(Or))
    // private lazy val uExpr: Parsley[Expr] = chain.prefix(notBOper)("!".as(Negate) | "-".as(Invert) | "len".as(Len) | "ord".as(Ord) | "chr".as(Chr))                  

    // private lazy val notUOper: Parsley[Expr] = bExpr| atom
    // private lazy val notBOper: Parsley[Expr] = uExpr| atom

    private lazy val operators: Parsley[Expr] = precedence(atom, atom)(
        Ops(Prefix)("!" as Invert,"-" #> Negate,"len" #> Len,"ord" #> Ord,"chr" #> Chr),
        Ops(InfixL)("*" #> Mul, "/" #> Div, "%" #> Mod, "+" #> Add, "-" #> Sub, ">=" #> GreaterThanEq, "<=" #> LessThanEq,
        ">" #> GreaterThan, "<" #> LessThan, "==" #> Eq, "!=" #> NotEq, "&&" #> And, "||" #> Or),
    )

    // private lazy val bExpr: Parsley[Expr] = chain.left1(atom)("+".as(Add))
    // private lazy val uExpr = chain.left1(notUOper)("!" #> UOper("!") | "-"  #> UOper("-") | "len" #> UOper("len") |  "ord" #> UOper("ord")| "chr" #> UOper("chr"))
    // private lazy val bExpr = chain.prefix(notBOper)("+" #> BOper("+")| "-" #> BOper("-")| "*" #> BOper("*")| "/" #> BOper("/")| "%" #> BOper("%")| "<" #> BOper("<")| ">" #> BOper(">")| "<=" #> BOper("<=" )| ">=" #> BOper(">=")| "==" #> BOper("==")| "!=" #> BOper("!=")| "&&" #> BOper("&&")| "||" #> BOper("||"))
    // UnaryOperation.lift(uOper, expr) |

    // -----------------------------
    private lazy val atom : Parsley[Expr] = atomic(ident <~ notFollowedBy("[")) | arr | intLiter | boolLiter | charLiter | stringLiter | pairLiter 
    private lazy val uOper: Parsley[UOper]  =  "!" #> UOper("!") | "-"  #> UOper("-") | "len" #> UOper("len") |  "ord" #> UOper("ord")| "chr" #> UOper("chr")
    private lazy val bOper: Parsley[BOper] = "*" #> BOper("*")| "/" #> BOper("/")| "%" #> BOper("%")| "+" #> BOper("+")| 
                            "-" #> BOper("-")| "<" #> BOper("<")| ">" #> BOper(">")| "<=" #> BOper("<=" )| 
                            ">=" #> BOper(">=")| "==" #> BOper("==")| "!=" #> BOper("!=")| "&&" #> BOper("&&")| "||" #> BOper("||")
    private lazy val arr = ArrElem.lift(ident, some("[" ~> expr <~ "]"))
}
