package wacc

import parsley.{Failure, Result, Success}
import ast._
import parser._
import scala.language.implicitConversions

class ParserCombinatorTest extends ParserTest { 
// -------------------------- valid Test  --------------------------

// ----------- statement parser test -----------

it should "parse program" in {
    parse("begin skip end") shouldBe Success(Program(List(), Skip))
}

it should "parse program with function" in {
    parse("begin int f(int x) is return x + 1 end skip end") shouldBe Success(Program(List(Func(BaseType("int"), Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), ByteIntLiter(1))))), Skip))
}
it should "parse program with function2" in {
    parse ("begin int f(int x) is return x + 2 end skip end") shouldBe Success(Program(List(Func(BaseType("int"), Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), ByteIntLiter(2))))), Skip))
}
it should "parse function" in {
    funcParse("int f(int x) is return x + 1 end") shouldBe  Success(Func(BaseType("int"), Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), ByteIntLiter(1)))))
}

it should "parse void function" in {
    funcParse("void f(int x) is exit x end") shouldBe  Success(Func(VoidType, Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Exit(Ident("x"))))
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
    stmtParse("x = 5") shouldBe Success(Assignment(Ident("x"), ByteIntLiter(5)))

    // If statement
    stmtParse("if true then skip else skip fi") shouldBe Success(If(BoolLiter(true), Skip, Skip))

    // While statement
    stmtParse("while x do skip done") shouldBe Success(While(Ident("x"), Skip))

    // Print statement
    stmtParse("print x") shouldBe Success(Print(Ident("x"), false))
    stmtParse(("print a[i]")) shouldBe Success(Print(ArrElem(Ident("a"), List(Ident("i"))), false))

    // Println statement    
    stmtParse("println x") shouldBe Success(Print(Ident("x"), true))
    stmtParse("println (a)") shouldBe Success(Print(Ident("a"), true))
    stmtParse("println 1-+2") shouldBe Success(Print(Sub(ByteIntLiter(1), ByteIntLiter(2)), true))

    // Begin statement
    stmtParse("begin skip end") shouldBe Success(Begin(Skip))

    // Read statement
    stmtParse("read x") shouldBe Success(Read(Ident("x")))

    // Free statement
    stmtParse("free x") shouldBe Success(Free(Ident("x")))

    // Return statement
    stmtParse("return x") shouldBe Success(Return(Ident("x")))

    // Exit statement
    stmtParse("exit x") shouldBe Success(Exit(Ident("x")))

    // New assignment statement
    stmtParse("int x = 5") shouldBe Success(NewAssignment(BaseType("int"), Ident("x"), ByteIntLiter(5)))

    // New pair assignment statement
    stmtParse("pair(int, bool) x = newpair(1, true)") shouldBe Success(NewAssignment(PairType(BaseType("int"), BaseType("bool")), Ident("x"), NewPairRValue(ByteIntLiter(1), BoolLiter(true))))
    stmtParse("pair(pair, pair) x = newpair(1, true)") shouldBe Success(NewAssignment(PairType(PairTypeElem, PairTypeElem), Ident("x"), NewPairRValue(ByteIntLiter(1), BoolLiter(true))))

    // Seq statement
    stmtParse("skip; skip") shouldBe Success(SeqStmt(Skip, Skip))

    // Seq statement
    stmtParse("skip; skip; skip") shouldBe Success(SeqStmt(Skip, SeqStmt(Skip, Skip)))
}

it should "parse lValue" in {
    // Ident lValue
    lValueParse("x") shouldBe Success(Ident("x"))

    // Pair element lValue
    lValueParse("fst x") shouldBe Success(FstPairElem(Ident("x")))
    lValueParse("snd fst x") shouldBe Success(SndPairElem(FstPairElem(Ident("x"))))
}

it should "parse rValue" in {
    // Expr rValue
    rValueParse("x") shouldBe Success(Ident("x"))

    // Array literal rValue
    rValueParse("[1, 2, 3]") shouldBe Success(ArrLiter(ByteIntLiter(1), List(ByteIntLiter(2), ByteIntLiter(3))))

    // New pair rValue
    rValueParse("newpair(1, 2)") shouldBe Success(NewPairRValue(ByteIntLiter(1), ByteIntLiter(2)))

    // Pair element rValue
    rValueParse("fst x") shouldBe Success(FstPairElem(Ident("x")))
    // Call rValue
    rValueParse("call f(1, 2)") shouldBe Success(CallRValue(Ident("f"), ArgList(List(ByteIntLiter(1), ByteIntLiter(2)))))
    rValueParse("call f(true)") shouldBe Success(CallRValue(Ident("f"), ArgList(List(BoolLiter(true)))))
    rValueParse("call f(1, true, 3)") shouldBe Success(CallRValue(Ident("f"), ArgList(List(ByteIntLiter(1), BoolLiter(true), ByteIntLiter(3)))))
}

it should "parse argsList" in {
    argsListParse("1, 2, 3") shouldBe Success(ArgList(List(ByteIntLiter(1), ByteIntLiter(2), ByteIntLiter(3))))
}


it should "parse arrl" in {
    arrlParse("x[1]") shouldBe Success(ArrElem(Ident("x"), List(ByteIntLiter(1))))
}

it should "parse arrLiter" in {
    arrLiterParse("[]") shouldBe Success(ArrLiter(StringLiter("empty"), List()))
    arrLiterParse("[1]") shouldBe Success(ArrLiter(ByteIntLiter(1), List()))
    arrLiterParse("[1, 2, 3]") shouldBe Success(ArrLiter(ByteIntLiter(1), List(ByteIntLiter(2), ByteIntLiter(3))))
}

// ----------- Expression Parser Tests -----------

  it should "parse expression" in {

    // Literal expression
    exprParse("1") shouldBe Success(ByteIntLiter(1))

    // Binary operation
    exprParse("1 + 2") shouldBe Success(Add(ByteIntLiter(1), ByteIntLiter(2)))
    exprParse("1 - 2 - 5") shouldBe Success(Sub(Sub(ByteIntLiter(1), ByteIntLiter(2)), ByteIntLiter(5)))
    // exprParse("p && q && r") shouldBe Success(And(Ident("p"),(And(Ident("q"), Ident("r")))))
    
    // Unary operation
    exprParse("-1") shouldBe Success(Negate(ByteIntLiter(1)))

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
    allTypeParse("pair(int, bool)") shouldBe Success(PairType(BaseType("int"), BaseType("bool")))
    allTypeParse("pair(int[], bool[])") shouldBe Success(PairType(ArrayType(BaseType("int")), ArrayType(BaseType("bool"))))
    allTypeParse("pair(int, bool[])") shouldBe Success(PairType(BaseType("int"), ArrayType(BaseType("bool"))))
    allTypeParse("pair(int[], bool)") shouldBe Success(PairType(ArrayType(BaseType("int")), BaseType("bool")))
    allTypeParse("pair(pair(int, bool)[], pair(bool, int)[])") shouldBe Success(PairType(ArrayType(PairType(BaseType("int"), BaseType("bool"))), ArrayType(PairType(BaseType("bool"), BaseType("int")))))
    allTypeParse("pair(int, pair(int, char)[])") shouldBe Success(PairType(BaseType("int"), ArrayType(PairType(BaseType("int"), BaseType("char"))))
    ) 
  }
}









