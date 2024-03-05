package wacc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import parser._
import ast._

class FunctionOverloadingTest extends AnyFlatSpec with Matchers {
  val symbolTable = new SymbolTable
  val semanticsChecker = new semanticsChecker(new SymbolTable)

  it should "detect function overloading (program aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val prog1 = Program(List(Func(BaseType("int"), Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(1))))), Skip)
    val prog2 = Program(List(Func(BaseType("int"), Ident("f"), ParamList(List(Param(BaseType("int"), Ident("x")))), Return(Add(Ident("x"), IntLiter(2))))), Skip)
    // Semantic check each function
    semanticsChecker.semanticCheck(prog1)
    semanticsChecker.semanticCheck(prog2)

    val symbolEntry1 = symbolTable.lookupSymbol(Ident("f")).getOrElse(fail("Function not found"))
    val symbolEntry2 = symbolTable.lookupSymbol(Ident("f")).getOrElse(fail("Function not found"))

    assert(symbolEntry1.varType == "func" && symbolEntry2.varType == "func")
  }

  

  it should "detect function overloading (function aspect)" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("bool"), Ident("add"), ParamList(List(Param(BaseType("bool"), Ident("b")))), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    val symbolEntry1 = symbolTable.lookupSymbol(Ident("add")).getOrElse(fail("Function not found"))
    val symbolEntry2 = symbolTable.lookupSymbol(Ident("add")).getOrElse(fail("Function not found"))

    val error = semanticsChecker.getSemanticErrors
    print(error)

    assert(symbolEntry1.varType == "func" && symbolEntry2.varType == "func")
    assert(symbolEntry1.value.length == 2)
    assert(symbolEntry2.value.length == 2)
  }

  it should "detect function overloading (function call aspect)" ignore{
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

    val error = semanticsChecker.getSemanticErrors
    print(error)

    assert(error.length == 1)
  }

  it should "detect function overloading (function call aspect) with different parameter types" ignore {
    // Create two functions with the same name but different parameter types
    val func1 = Func(BaseType("int"), Ident("add"), ParamList(List(Param(BaseType("int"), Ident("a")))), Skip)
    val func2 = Func(BaseType("bool"), Ident("add"), ParamList(List(Param(BaseType("bool"), Ident("b")))), Skip)

    // Semantic check each function
    semanticsChecker.semanticCheck(func1)
    semanticsChecker.semanticCheck(func2)

    // Create a function call with the same name as the two functions but with different parameter types
    val funcCall = CallRValue(Ident("add"), ArgList(List(BoolLiter(true))))

    // Semantic check the function call
    semanticsChecker.semanticCheck(funcCall)

    val error = semanticsChecker.getSemanticErrors
    print(error)

    assert(error.length == 1)
  }
}