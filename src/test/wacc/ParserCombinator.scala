package wacc

import parsley.{Failure, Result, Success}
import ast._
import parser._
import scala.language.implicitConversions

class ParserCombinatorTest extends ParserTest { 

// ----------- statement parser test -----------

  it should "parse program" in {
    parse("begin skip end") shouldBe Success(Program(List(), Skip))
}

it should "parse function" in {
    funcParse("int f(int x) is skip end") shouldBe Success(Func(BaseType("int"), Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Skip))
}

it should "parse paramList" in {
  paramListParse("(int x, bool y)") shouldBe Success(ParamList(List(Param(BaseType("int"), Ident("x")), Param(BaseType("bool"), Ident("y")))))
}

it should "parse param" in {
  paramParse("int x") shouldBe Success(Param(BaseType("int"), Ident("x")))
}


it should "parse statement" in {
    // Skip statement
    stmtParse("skip") shouldBe Success(Skip)

    // Assignment statement
    stmtParse("x = 5") shouldBe Success(Assignment(IdentLValue(Ident("x")), ExprRValue(IntLiter(5))))

    // If statement
    stmtParse("if true then skip else skip") shouldBe Success(If(BoolLiter(true), Skip, Skip))

    // While statement
    stmtParse("while x do skip done") shouldBe Success(While(Ident("x"), Skip))

    // Print statement
    stmtParse("print x") shouldBe Success(Print(Ident("x"), false))

    // Println statement    
    stmtParse("println x") shouldBe Success(Print(Ident("x"), true))

    // Begin statement
    stmtParse("begin skip end") shouldBe Success(Begin(Skip))

    // Read statement
    stmtParse("read x") shouldBe Success(Read(IdentLValue(Ident("x"))))

    // Free statement
    stmtParse("free x") shouldBe Success(Free(Ident("x")))

    // Return statement
    stmtParse("return x") shouldBe Success(Return(Ident("x")))

    // Exit statement
    stmtParse("exit x") shouldBe Success(Exit(Ident("x")))

    // New assignment statement
    stmtParse("int x = 5") shouldBe Success(NewAssignment(BaseType("int"), Ident("x"), ExprRValue(IntLiter(5))))

    // New pair assignment statement
    stmtParse("pair(int, bool) x = newpair(1, true)") shouldBe Success(NewAssignment(PairType(BaseTypeElem("int"), BaseTypeElem("bool")), Ident("x"), NewPairRValue(IntLiter(1), BoolLiter(true))))

    // // Seq statement
    // parser.parse("skip; skip") shouldBe Success(SeqStmt(Skip, Skip))

    // // Seq statement
    // parser.parse("skip; skip; skip") shouldBe Success(SeqStmt(Skip, SeqStmt(Skip, Skip)))
}

it should "parse lValue" in {
    // Ident lValue
    lValueParse("x") shouldBe Success(IdentLValue(Ident("x")))

    // Array element lValue
    lValueParse("x[1]") shouldBe Success(ArrElemLValue(Ident("x"), List(IntLiter(1))))

    // Pair element lValue
    lValueParse("fst x") shouldBe Success(PairElem("fst", IdentLValue(Ident("x"))))
}

it should "parse rValue" in {
    // Expr rValue
    rValueParse("x") shouldBe Success(ExprRValue(Ident("x")))

    // Array literal rValue
    rValueParse("[1, 2, 3]") shouldBe Success(ArrayLiterRValue(ArrLiter(IntLiter(1), List(IntLiter(2), IntLiter(3)))))

    // New pair rValue
    rValueParse("newpair(1, 2)") shouldBe Success(NewPairRValue(IntLiter(1), IntLiter(2)))

    // Pair element rValue
    rValueParse("fst x") shouldBe Success(PairElem("fst", IdentLValue(Ident("x"))))

    // Call rValue
    rValueParse("call f(1, 2)") shouldBe Success(CallRValue(Ident("f"), ArgList(List(IntLiter(1), IntLiter(2)))))
}

it should "parse argsList" in {
    argsListParse("1, 2, 3") shouldBe Success(ArgList(List(IntLiter(1), IntLiter(2), IntLiter(3))))
}

it should "parse pairElem" in {
    pairElemParse("fst x") shouldBe Success(PairElem("fst", IdentLValue(Ident("x"))))
}

it should "parse arrl" in {
    arrlParse("x[1]") shouldBe Success(ArrElemLValue(Ident("x"), List(IntLiter(1))))
}

it should "parse arrLiter" in {
    arrLiterParse("[]") shouldBe Success(ArrLiter(null, List()))
    arrLiterParse("[1, 2, 3]") shouldBe Success(ArrLiter(IntLiter(1), List(IntLiter(2), IntLiter(3))))
}

// ----------- Expression Parser Tests -----------

  it should "parse expression" in {

    // Literal expression
    exprParse("1") shouldBe Success(IntLiter(1))

    // Binary operation
    exprParse("1 + 2") shouldBe Success(BinaryOperation(BOper("+"), IntLiter(1), IntLiter(2)))

    // Unary operation
    exprParse("-1") shouldBe Success(UnaryOperation(UOper("-"), IntLiter(1)))

    // atom expression
    exprParse("x") shouldBe Success(Ident("x"))

  }

// ----------- Type Parser Tests -----------
  it should "parse all type" in {
    allTypeParse("int") shouldBe Success(BaseType("int"))
    allTypeParse("bool") shouldBe Success(BaseType("bool"))
    allTypeParse("char") shouldBe Success(BaseType("char"))
    allTypeParse("string") shouldBe Success(BaseType("string"))
    allTypeParse("int[]") shouldBe Success(ArrayType(BaseType("int")))
    allTypeParse("bool[]") shouldBe Success(ArrayType(BaseType("bool")))
    allTypeParse("char[]") shouldBe Success(ArrayType(BaseType("char")))
    allTypeParse("string[]") shouldBe Success(ArrayType(BaseType("string")))
    allTypeParse("pair(int, bool)") shouldBe Success(PairType(BaseTypeElem("int"), BaseTypeElem("bool")))
    allTypeParse("pair(int[], bool[])") shouldBe Success(PairType(ArrayTypeElem(ArrayType(BaseType("int"))), ArrayTypeElem(ArrayType(BaseType("bool")))))
    allTypeParse("pair(int, bool[])") shouldBe Success(PairType(BaseTypeElem("int"), ArrayTypeElem(ArrayType(BaseType("bool")))))
    allTypeParse("pair(int[], bool)") shouldBe Success(PairType(ArrayTypeElem(ArrayType(BaseType("int"))), BaseTypeElem("bool")))
  }
}









