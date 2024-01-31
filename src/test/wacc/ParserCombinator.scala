package wacc

import parsley.{Failure, Result, Success}
import ast._
import parser._
import scala.language.implicitConversions

class ParserCombinatorTest extends ParserTest { 
// ----------- Statement Parser Tests -----------
it should "parse statement" in {
    // Skip statement
    parser.parse("skip") shouldBe Success(Skip)

    // Assignment statement
    parser.parse("x = 5") shouldBe Success(Assignment(IdentLValue(Ident("x")), ExprRValue(IntLiter(5))))

    // If statement
    parser.parse("if true then skip else skip") shouldBe Success(If(BoolLiter(true), Skip, Skip))

    // While statement
    parser.parse("while x do skip done") shouldBe Success(While(Ident("x"), Skip))

    // Print statement
    parser.parse("print x") shouldBe Success(Print(Ident("x"), false))

    // Println statement    
    parser.parse("println x") shouldBe Success(Print(Ident("x"), true))

    // Begin statement
    parser.parse("begin skip end") shouldBe Success(Begin(Skip))

    // Read statement
    parser.parse("read x") shouldBe Success(Read(IdentLValue(Ident("x"))))

    // Free statement
    parser.parse("free x") shouldBe Success(Free(Ident("x")))

    // Return statement
    parser.parse("return x") shouldBe Success(Return(Ident("x")))

    // Exit statement
    parser.parse("exit x") shouldBe Success(Exit(Ident("x")))

    // New assignment statement
    parser.parse("int x = 5") shouldBe Success(NewAssignment(BaseType("int"), Ident("x"), ExprRValue(IntLiter(5))))

    // New pair assignment statement
    parser.parse("pair(int, bool) x = newpair(1, true)") shouldBe Success(NewAssignment(PairType(BaseTypeElem("int"), BaseTypeElem("bool")), Ident("x"), NewPairRValue(IntLiter(1), BoolLiter(true))))

    // // Seq statement
    // parser.parse("skip; skip") shouldBe Success(SeqStmt(Skip, Skip))

    // // Seq statement
    // parser.parse("skip; skip; skip") shouldBe Success(SeqStmt(Skip, SeqStmt(Skip, Skip)))



}
// ----------- Expression Parser Tests -----------

  it should "parse expression" in {

    // Literal expression
    parser.parse("123") shouldBe Success(IntLiter(123))

    // Binary operation
    parser.parse("1 + 2") shouldBe Success(BinaryOperation(BOper("+"), IntLiter(1), IntLiter(2)))

    // Unary operation
    parser.parse("!true") shouldBe Success(UnaryOperation(UOper("!"), BoolLiter(true)))

    // atom expression
    

  }

// ----------- Type Parser Tests -----------
  it should "parse type" in {

    // Base type
    parser.parse("int") shouldBe Success(BaseType("int"))

    // Array type
    parser.parse("int[]") shouldBe Success(ArrayType(BaseType("int")))

    // Pair type
    parser.parse("pair(int, bool)") shouldBe Success(PairType(BaseTypeElem("int"), BaseTypeElem("bool")))
  }
}




