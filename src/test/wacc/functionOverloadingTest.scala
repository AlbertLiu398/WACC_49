package wacc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import parser._
import ast._

class FunctionOverloadingTest extends AnyFlatSpec with Matchers {
  val symbolTable = new SymbolTable
  val semanticsChecker = new semanticsChecker(symbolTable)

  it should "function overloading (function declare aspect)" in {
    val program = Program(List(
    Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1)))),
    Func(BaseType("bool"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))
  ), Skip)

    // Semantic check the program
    semanticsChecker.semanticCheck(program)
    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("Function return type does not match its return type"))
    semanticsChecker.getSemanticErrors.length shouldBe 1
    cleanForNextTest()
  }


    it should "function not overloading (function declare aspect)" in {
    val program = Program(List(
    Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1)))),
    Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))
  ), Skip)
    semanticsChecker.semanticCheck(program)

    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("y: ambiguous function declare with same name, parameters and return type "))
    semanticsChecker.getSemanticErrors.length shouldBe 1
    cleanForNextTest()
  }


  it should "function not overloading (function aspect)" in {
    // Create two same functions
    val program = Program(List(
    Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Return(Add(Ident("a"), IntLiter(1)))),
    Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Return(Add(Ident("a"), IntLiter(1))))
  ), Skip)
  

    // Semantic check each function
    semanticsChecker.semanticCheck(program)

    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("add: ambiguous function declare with same name, parameters and return type "))
    cleanForNextTest()
  }


 
  it should "matching overloadedFunction (function call aspect)" in {
    // Create two functions with the same name but different parameter types and return types

    val program = Program(List(
      Func(BaseType("bool"),Ident("f1"),ParamList(List(Param(BaseType("bool"),Ident("x")))),Return(Ident("x"))),
      Func(BaseType("int"),Ident("f1"),ParamList(List(Param(BaseType("int"),Ident("x")))),Return(Ident("x")))),
      SeqStmt(NewAssignment(BaseType("int"),Ident("y"),IntLiter(0)),Assignment(Ident("y"),CallRValue(Ident("f1"),ArgList(List(IntLiter(1)))))))

     // Semantic check each function
    semanticsChecker.semanticCheck(program)

    semanticsChecker.getSemanticErrors shouldBe List()
    cleanForNextTest()
  }

  it should "matching non overloadedFunction (function call aspect)" in {
    val program = Program(List(
      Func(BaseType("bool"),Ident("f1"),ParamList(List(Param(BaseType("bool"),Ident("x")))),Return(Ident("x"))),
      Func(BaseType("int"),Ident("f1"),ParamList(List(Param(BaseType("int"),Ident("x")))),Return(Ident("x")))),
      SeqStmt(NewAssignment(BaseType("int"),Ident("y"),IntLiter(0)),Assignment(Ident("y"),CallRValue(Ident("f2"),ArgList(List(IntLiter(1)))))))
    semanticsChecker.semanticCheck(program)
    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("Function does not exist"), SemanticError("Assignment type mismatch"))
    cleanForNextTest()
  }


  it should "overloadedFunction : not match argsType (function call aspect)" in {
    val program = Program(List(
      Func(BaseType("bool"),Ident("f1"),ParamList(List(Param(BaseType("bool"),Ident("x")))),Return(Ident("x"))),
      Func(BaseType("int"),Ident("f1"),ParamList(List(Param(BaseType("int"),Ident("x")))),Return(Ident("x")))),
      SeqStmt(NewAssignment(BaseType("int"),Ident("y"),IntLiter(0)),Assignment(Ident("y"),CallRValue(Ident("f1"),ArgList(List(CharLiter('a')))))))
    semanticsChecker.semanticCheck(program)
    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("No matching function overload"), SemanticError("Assignment type mismatch"))
    cleanForNextTest()
  }



  private def cleanForNextTest(): Unit = {
    symbolTable.clean()
    semanticsChecker.refreshSymbolTable()
  }
}
