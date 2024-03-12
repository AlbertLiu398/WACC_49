package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.{Failure, Result, Success}

import parser._
import ast._

class VoidTypeTest extends AnyFlatSpec with Matchers {
    val symbolTable = new SymbolTable
    val semanticsChecker = new semanticsChecker(symbolTable)

    it should "parse voidType function" in {
        val result = parser.parse("begin void f() is return end skip end")
        result shouldBe Success(Program(List(Func(VoidType, Ident("f"), ParamList(List()), ReturnVoid)), Skip))
    }

    it should "parse voidType function with parameters" in {
        val result = parser.parse("begin void f(int x, bool y) is return end skip end")
        result shouldBe Success(Program(List(Func(VoidType, Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")), Param(BaseType("bool"), Ident("y")))), ReturnVoid)), Skip))
    }

    it should "parse voidType: void function should not return value" in {
        val result = parser.parse("begin void f() is return 1 end skip end")
        semanticsChecker.semanticCheck(result.get)
        semanticsChecker.getSemanticErrors shouldBe List(SemanticError("Void function cannot return a value"), SemanticError("Function return type does not match its return type"))
        cleanForNextTest()
    }

    it should "parse voidType: void function does not return value" in {
        val result = parser.parse("begin void f() is return end skip end")
        semanticsChecker.semanticCheck(result.get)
        semanticsChecker.getSemanticErrors shouldBe List()
        cleanForNextTest()
    }

     it should "parse voidType: void function end up with exit " in {
        val result = parser.parse("begin void f(int x) is exit x end skip end")
        semanticsChecker.semanticCheck(result.get)
        semanticsChecker.getSemanticErrors shouldBe List()
        cleanForNextTest()
    }

    

    private def cleanForNextTest(): Unit = {
        symbolTable.clean()
        semanticsChecker.refreshSymbolTable()
    }
}
