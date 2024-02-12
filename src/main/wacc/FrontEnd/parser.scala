package wacc

import parsley.{Parsley}
import parsley.expr._
import parsley.Parsley._
import parsley.syntax._
import scala.language.postfixOps
import parsley.debug, debug._ 
import lexer.implicits.implicitSymbol
import lexer._
import ast._
import parsley.errors.combinator.ErrorMethods

object parser {
    import parsley.syntax.lift.{Lift1, Lift2, Lift3, Lift4}

    // -------------- each Parser used for unit test --------------

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

    // -------------------------- Literals -------------------------
    private lazy val intLiter = (integer.map(IntLiter) | 
                               "+" ~> integer.map(IntLiter) | 
                               "-" ~> integer.map(IntLiter)).label("intLiter").explain("Integer needed")

    private lazy val ident = identifier.map(Ident)
    private lazy val boolLiter = "true" #> BoolLiter(true) | 
                                 "false" #> BoolLiter(false)
                                 
    private lazy val charLiter =  graphicCharacter.map(CharLiter)
    private lazy val stringLiter = lexer.string.map(StringLiter)
    private lazy val pairLiter = "null" #> PairLiter

   // --------------------------  Program -------------------------
    private lazy val prog: Parsley[Program] = Program.lift("begin" ~> many(func), stmt <~ "end")


    // ------------------------- Functions -------------------------
    private lazy val func: Parsley[Func] = atomic(Func.lift(allType, ident, "("~> ParamList.lift(pure(List())) <~")", "is" ~> funcStmt <~ "end")) |
                                           atomic(Func.lift(allType, ident, paramList, "is" ~> funcStmt <~ "end"))   

    // ------------------------- ParamList -------------------------                             
    private lazy val paramList: Parsley[ParamList] = "(" ~> ParamList.lift(commaSep1_(param)) <~ ")"
    private lazy val param = Param.lift(allType, ident)

    // ------------------------- Overall Statements ------------------ 
    private lazy val stmt = (atomic( stmtAtom <~ notFollowedBy(";"))| stmtJoin).label("stmt").explain("Statement needed")
    private lazy val stmtJoin: Parsley[Stmt] = SeqStmt.lift(stmtAtom <~ ";", stmt)
    private lazy val stmtAtom : Parsley[Stmt] = 
        "skip" #> Skip|
        NewAssignment.lift(allType, ident, "="~> rValue) |
        Assignment.lift(lValue, "=" ~> rValue) |
        "read" ~> Read.lift(lValue) |
        "free" ~> Free.lift(expr) |
        "print" ~> Print.lift(expr, pure(false)) |
        "println" ~> Print.lift(expr, pure(true)) |
        "exit" ~> Exit.lift(expr) |
        "return" ~> Return.lift(expr) |
        If.lift("if" ~> expr, "then" ~> stmt, "else" ~> stmt <~ "fi")|
        While.lift("while" ~> expr, "do" ~> stmt <~ "done") |
        Begin.lift("begin" ~> stmt <~ "end")

                //  -------------  Statement (for function only) -----------
    // funcStmt : used to check function statement is valid 
    private lazy val funcStmt = stmt.filter(checkTermination).label ("funcStmt").explain("Function statement needed")
    
    // checkTermination : used to check if this statement contains terminating(exit, return) statement
    def checkTermination(stmt: Stmt): Boolean = stmt match {
        case SeqStmt(_, stmt) => checkTermination(stmt)
        case Begin(stmt) => checkTermination(stmt)
        case If(_, thenStmt, elseStmt) => checkTermination(thenStmt) && checkTermination(elseStmt)
        case While(_, stmt) => checkTermination(stmt)
        case Exit(_) => true
        case Return(_) => true
        case _ => false
    }

    // -------------------------- LValue -------------------------
    private lazy val lValue: Parsley[LValue] = (pairElem | atomic(ident <~ notFollowedBy("[")) | arr).label("lValue").explain("LValue needed")


    // -------------------------- RValue -------------------------
    private lazy val rValue =
        (NewPairRValue.lift("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") |
        CallRValue.lift("call" ~> ident, "(" ~> argsList <~ ")") |
        pairElem |
        expr|
        arrLiter).label("rValue").explain("RValue needed")

      // -------------------------- Rvalue -- PairElem ------------------------
    private lazy val notPairElem: Parsley[LValue] = (atomic(ident<~ notFollowedBy("[")) | arr)
    private lazy val notFirstPairElem: Parsley[LValue] = sndPairElem | notPairElem
    private lazy val notSecondPairElem: Parsley[LValue] = fstPairElem | notPairElem

    private lazy val pairElem = fstPairElem | sndPairElem
    private lazy val fstPairElem = chain.prefix1(notFirstPairElem)("fst".as(FstPairElem))
    private lazy val sndPairElem = chain.prefix1(notSecondPairElem)("snd".as(SndPairElem))  

       // --------------------------- Rvalue --- Array Literals ----------------------- 
    private lazy val arrLiter: Parsley[ArrLiter] 
      = "[]" #> ArrLiter(StringLiter("empty"), List()) | 
         atomic(ArrLiter.lift("[" ~> expr <~ notFollowedBy(",") <~ "]", pure(List()))) |
         ArrLiter.lift("[" ~> expr <~ ",", commaSep_(expr) <~ "]")
    private lazy val exprOrArrayLit: Parsley[Expr] = expr | arrLiter

    // -------------------------- ArgList -------------------------
    private lazy val argsList: Parsley[ArgList] = (ArgList.lift(commaSep1_(exprOrArrayLit)) |  ArgList.lift(pure(List()))).label("argsList").explain("Argument List needed")


    // -------------------------- Types -------------------------------- 
    private lazy val allType: Parsley[Type] = (atomic(arrayType) | notArrayType).label("type").explain("Type needed")
    private lazy val notArrayType: Parsley[Type] = baseType | pairType 

    private lazy val baseType = "int" ~> BaseType.lift(pure("int")) | "bool" ~> BaseType.lift(pure("bool")) | "char" ~> BaseType.lift(pure("char")) | "string" ~> BaseType.lift(pure("string"))
    private lazy val arrayType = chain.postfix1(notArrayType)("[]".as(ArrayType))
    private lazy val pairType: Parsley[Type] = "pair" ~> "(" ~> PairType.lift(pairElemType, "," ~> pairElemType <~ ")")

    private lazy val pairElemType: Parsley[PairElemType] =  pairTypeElem| atomic(baseType <~ notFollowedBy("[")) | arrayType
    private lazy val pairTypeElem: Parsley[PairElemType] = atomic("pair" #> PairTypeElem <~ notFollowedBy("("))

    // -------------------------- Expressions --------------------------
    
    private lazy val expr: Parsley[Expr]= (atomic(operators) | atom).label("expression").explain("expression needed")
    private lazy val operators: Parsley[Expr] = (precedence(atom)(
        Ops(Prefix)("-" #> Negate, "!" #> Invert, "len" #> Len, "ord" #> Ord, "chr" #> Chr),
        Ops(InfixL)("*" #> Mul, "/" #> Div, "%" #> Mod),
        Ops(InfixL)("+" #> Add, "-" #> Sub),
        Ops(InfixN)(">=" #> GreaterThanEq, "<=" #> LessThanEq, ">" #> GreaterThan, "<" #> LessThan),
        Ops(InfixN)("==" #> Eq, "!=" #> NotEq),
        Ops(InfixR)("&&" #> And),
        Ops(InfixR)("||" #> Or),   
    )).label("expression").explain("expression needed")

    private lazy val atom : Parsley[Expr] = "(" ~> expr <~ ")"| atomic(ident <~ notFollowedBy("[")) | arr | intLiter | boolLiter | charLiter | stringLiter| pairLiter 


    private lazy val uOper: Parsley[UOper]  =  "!" #> UOper("!") | "-"  #> UOper("-") | "len" #> UOper("len") |  "ord" #> UOper("ord")| "chr" #> UOper("chr")
    private lazy val bOper: Parsley[BOper] = "*" #> BOper("*")| "/" #> BOper("/")| "%" #> BOper("%")| "+" #> BOper("+")| 
                            "-" #> BOper("-")| "<" #> BOper("<")| ">" #> BOper(">")| "<=" #> BOper("<=" )| 
                            ">=" #> BOper(">=")| "==" #> BOper("==")| "!=" #> BOper("!=")| "&&" #> BOper("&&")| "||" #> BOper("||")
    // -------------------------- Array element --------------------------         
    private lazy val arr = (ArrElem.lift(ident, some("[" ~> expr <~ "]"))).label("Array").explain("Array needed ")
}
