package wacc
import ast._
import scala.language.implicitConversions

class  SemanticCheckerTest extends ParserTest {
    val semanticschecker = new semanticsChecker(new SymbolTable)

    it should "semantic check BinaryOperator" in {
        it should "semantic check Add" in {
            semanticschecker.semanticCheck(Add(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check Sub" in {
            semanticschecker.semanticCheck(Sub(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check Mul" in {
            semanticschecker.semanticCheck(Mul(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check Div" in {
            semanticschecker.semanticCheck(Div(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check Mod" in {
            semanticschecker.semanticCheck(Mod(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check LessThan" in {
            semanticschecker.semanticCheck(LessThan(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check LessThanEq" in {
            semanticschecker.semanticCheck(LessThanEq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check GreaterThan" in {
            semanticschecker.semanticCheck(GreaterThan(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check GreaterThanEq" in {
            semanticschecker.semanticCheck(GreaterThanEq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check Eq" in {
            semanticschecker.semanticCheck(Eq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check NotEq" in {
            semanticschecker.semanticCheck(NotEq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check And" in {
            semanticschecker.semanticCheck(And(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
        it should "semantic check Or" in {
            semanticschecker.semanticCheck(Or(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("expression type mismatch"))
        }
    }

    it should "semantic check PairElem" in {
        it should "semantic check fst" in {
            semanticschecker.semanticCheck(FstPairElem(Ident("x")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("Value not exist"))
        }
        it should "semantic check snd" in {
            semanticschecker.semanticCheck(SndPairElem(Ident("x")))
            semanticschecker.getSemanticErrors shouldBe List(SemanticError("Value not exist"))
        }
    }
}
