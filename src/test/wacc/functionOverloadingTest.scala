package wacc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import parser._
import ast._

class FunctionOverloadingTest extends AnyFlatSpec with Matchers {
  val symbolTable = new SymbolTable
  val semanticsChecker = new semanticsChecker(symbolTable)

  it should "function overloading (program aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val prog1 = Program(List(Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1)))), Func(BaseType("bool"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))), Skip)
    // Semantic check each function
    semanticsChecker.semanticCheck(prog1)
    symbolTable.lookupFunctionOverloads(Ident("y")).getOrElse(fail("Function not found")).length shouldBe 2
    cleanForNextTest()
  }

  it should "function not overloading (program aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val prog1 = Program(List(Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))), Skip)
    val prog2 = Program(List(Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))), Skip)
    // Semantic check each function
    semanticsChecker.semanticCheck(prog1)
    semanticsChecker.semanticCheck(prog2)

    val symbolEntry1 = symbolTable.lookupSymbol(Ident("y")).getOrElse(fail("Function not found"))
    val symbolEntry2 = symbolTable.lookupSymbol(Ident("y")).getOrElse(fail("Function not found"))

    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("ambiguous function with same name, parameters and return type"))
    cleanForNextTest()
  }

  it should "function overloading: different return type (function declare aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("bool"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val program = Program(List(func1, func2), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    val symbolEntry1 = symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found"))
    val symbolEntry2 = symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found"))

    symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found")).length shouldBe 2
    cleanForNextTest()
  }

    it should "function overloading: different param Type (function declare aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("bool"), Ident("b")))), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    val symbolEntry1 = symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found"))
    val symbolEntry2 = symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found"))

    symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found")).length shouldBe 2
    cleanForNextTest()
  }


  it should "function not overloading (function aspect)" ignore {
    // Create two functions with the same name but different parameter types
    // func1 : int add(int a) is skip end 
    // func2 : int add(bool b) is skip end
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    val symbolEntry1 = symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found"))
    val symbolEntry2 = symbolTable.lookupFunctionOverloads(Ident("add")).getOrElse(fail("Function not found"))

    val error = semanticsChecker.getSemanticErrors
    error shouldBe List(SemanticError("add: ambiguous function declare with same name, parameters and return type "))
    cleanForNextTest()
  }


  it should "function not overloading 2 (Func aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Sub(Ident("x"), IntLiter(1))))
    val func2 = Func(BaseType("int"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))
    val func3 = Func(BaseType("bool"), Ident("y"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))
    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)
    semanticsChecker.semanticCheck(func3)

    symbolTable.lookupFunctionOverloads(Ident("y")).getOrElse(fail("Function not found")).length shouldBe 2
    semanticsChecker.getSemanticErrors shouldBe List(SemanticError("y: ambiguous function declare with same name, parameters and return type "), SemanticError("Function return type does not match its return type"))
    cleanForNextTest()
  }

  it should "matching overloadedFunction (function call aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("bool"), Ident("add"), ParamList(List(Param(BaseType("bool"), Ident("b")))), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    // Create a function call with the same name as the two functions
    val funcCall = CallRValue(Ident("add"), ArgList(List(IntLiter(5))))

    // Semantic check the function call
    semanticsChecker.semanticCheck(funcCall)
    semanticsChecker.getSemanticErrors shouldBe List()
    funcCall.getType shouldBe "int"
    cleanForNextTest()
  }

  it should "matching non overloadedFunction (function call aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("bool"), Ident("add"), ParamList(List(Param(BaseType("bool"), Ident("a")))), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    // Create a function call with the same name as the two functions but with different parameter types
    val funcCall = CallRValue(Ident("add"), ArgList(List(BoolLiter(true))))

    // Semantic check the function call
    semanticsChecker.semanticCheck(funcCall)
    semanticsChecker.getSemanticErrors shouldBe List()
    funcCall.getType shouldBe "bool"
    cleanForNextTest()
  }


  private def cleanForNextTest(): Unit = {
    symbolTable.clean()
    semanticsChecker.refreshSymbolTable()
  }
}