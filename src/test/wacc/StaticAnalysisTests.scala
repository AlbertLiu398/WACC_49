package wacc

import ast._
import parser.stmtParse
import scala.language.implicitConversions
import java.beans.Expression

class StaticAnalysisTests extends ParserTest {

    val sC = new semanticsChecker(new SymbolTable)

    /*------------------- Array indexing tests -------------------*/

    it should "Static analysis: Array index negative" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[-2]").get)
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Negative array indexing"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Array index expression negative" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[3-7]").get)
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Negative array indexing"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array access" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[5]").get)
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Out of bounds array access"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array expression access" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[2*1]").get)
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Out of bounds array access"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array write" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; a[6] = 100").get)
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Out of bounds array write"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array expression write" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; a[6+7] = 100").get)
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Out of bounds array write"))
        sC.refreshSymbolTable()
    }

    /*------------------- Div by zero tests -------------------*/

    it should "Static analysis: Divide by zero" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Divide by zero expression" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Mod by zero" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Mod by zero expression" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    /*------------------- Null dereference tests -------------------*/

    it should "Static analysis: Free null" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Read null" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Set null" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Use null" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    /*------------------- Chr use tests -------------------*/

    it should "Static analysis: Chr use negative" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Chr use too big" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    /*------------------- Integer overflow tests -------------------*/

    it should "Static analysis: Integer overflow" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Integer underflow" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Integer multiplication overflow" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Integer negate overflow" in {
        sC.semanticCheck(null)
        sC.getSemanticErrors shouldBe List(SemanticError("<TODO>"))
        sC.refreshSymbolTable()
    }
}
