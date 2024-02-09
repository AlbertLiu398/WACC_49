package wacc

import ast._
import scala.language.implicitConversions
import java.beans.Expression

class  SemanticCheckerTest extends ParserTest {
    val semanticschecker = new semanticsChecker(new SymbolTable)

    // -------------------------- Expression --------------------------

    it should "semantic check Add" in {
        semanticschecker.semanticCheck(Add(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check Sub" in {
        semanticschecker.semanticCheck(Sub(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check Mul" in {
        semanticschecker.semanticCheck(Mul(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check Div" in {
        semanticschecker.semanticCheck(Div(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check Mod" in {
        semanticschecker.semanticCheck(Mod(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check LessThan" in {
        semanticschecker.semanticCheck(LessThan(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check LessThanEq" in {
        semanticschecker.semanticCheck(LessThanEq(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check GreaterThan" in {
        semanticschecker.semanticCheck(GreaterThan(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check GreaterThanEq" in {
        semanticschecker.semanticCheck(GreaterThanEq(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check Eq" in {
        semanticschecker.semanticCheck(Eq(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check NotEq" in {
        semanticschecker.semanticCheck(NotEq(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check And" in {
        semanticschecker.semanticCheck(And(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check Or" in {
        semanticschecker.semanticCheck(Or(IntLiter(1), StringLiter("1")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        semanticschecker.refreshSymbolTable()
    }

    it should "semantic check fst" in {
        semanticschecker.semanticCheck(FstPairElem(Ident("x")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("Ident not exist"), SemanticError("Value not exist"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check snd" in {
        semanticschecker.semanticCheck(SndPairElem(Ident("x")))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("Value not exist"))
        semanticschecker.refreshSymbolTable()
    }

    // -------------------------- Array --------------------------
    it should "semantic check array" in {
        semanticschecker.semanticCheck ((NewAssignment(ArrayType(BaseType("int")),Ident("should_be_int_array"),ArrLiter(CharLiter('a'),List(CharLiter('b'), CharLiter('c'))))))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("assignment type mismatch"))
        semanticschecker.refreshSymbolTable()
    }
    it should "semantic check array 2" in {
        semanticschecker.semanticCheck(ArrElem(Ident("x"), List(IntLiter(1), IntLiter(1))))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("array not exist"), SemanticError("too much indexing"))
        semanticschecker.refreshSymbolTable()
    }



    // -------------------------- Function --------------------------







    // -------------------------- If --------------------------


    it should "semantics check if condition" in {
        semanticschecker.semanticCheck((Program(List(),If(Add(IntLiter(1),IntLiter(1)),Skip,Skip))))
        semanticschecker.getSemanticErrors shouldBe List(SemanticError("condition need to be a boolean"))
        semanticschecker.refreshSymbolTable()
    }



    // -------------------------- Multiple --------------------------

    




    // -------------------------- Print --------------------------

    // it should "semantics check print type mismatch" in {
    //     semanticschecker.semanticCheck(())
    //     semanticschecker.getSemanticErrors shouldBe List(SemanticError(""))
    //     semanticschecker.refreshSymbolTable()
    // }
}
