package wacc
import ast._
import semanticsChecker._
import scala.language.implicitConversions

class  SemanticCheckerTest extends ParserTest {
    val semanticschecker = new semanticsChecker(new SymbolTable)

    it should "semantic check BinaryOperator" in {
        it should "semantic check Add" {
            semanticschecker.semanticCheck(Add(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check Sub" {
            semanticschecker.semanticCheck(Sub(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check Mul" {
            semanticschecker.semanticCheck(Mul(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check Div" {
            semanticschecker.semanticCheck(Div(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check Mod" {
            semanticschecker.semanticCheck(Mod(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check LessThan" {
            semanticschecker.semanticCheck(LessThan(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check LessThanEq" {
            semanticschecker.semanticCheck(LessThanEq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check GreaterThan" {
            semanticschecker.semanticCheck(GreaterThan(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check GreaterThanEq" {
            semanticschecker.semanticCheck(GreaterThanEq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check Eq" {
            semanticschecker.semanticCheck(Eq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check NotEq" {
            semanticschecker.semanticCheck(NotEq(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check And" {
            semanticschecker.semanticCheck(And(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
        it should "semantic check Or" {
            semanticschecker.semanticCheck(Or(IntLiter(1), StringLiter("1")))
            semanticschecker.getSemanticErrors shouldBe List("expression type mismatch")
        }
    }

    it should "semantic check PairElem" {
        it should "semantic check fst" {
            semanticschecker.semanticCheck(FstPairElem(Ident("x")))
            semanticschecker.getSemanticErrors shouldBe List("Value not exist")
        }
        it should "semantic check snd" {
            semanticschecker.semanticCheck(SndPairElem(Ident("x")))
            semanticschecker.getSemanticErrors shouldBe List("Value not exist")
        }
    }
}
