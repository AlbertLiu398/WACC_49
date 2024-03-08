package wacc

import ast._
import parser.stmtParse
import scala.language.implicitConversions
import java.beans.Expression

class StaticAnalysisTests extends ParserTest {

    val sC = new semanticsChecker(new SymbolTable)

    /*------------------- Div by zero tests -------------------*/

    it should "Static analysis: Divide by zero" in {
        sC.semanticCheck(stmtParse("int x = 10 / 0").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Divide by zero"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Divide by zero expression" in {
        sC.semanticCheck(stmtParse("int x = 6 / (4 - 4)").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Divide by zero"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Mod by zero" in {
         sC.semanticCheck(stmtParse("int x = 18 % 0").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Mod by zero"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Mod by zero expression" in {
         sC.semanticCheck(stmtParse("int x = 20 % (6 - 6)").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Mod by zero"))
        sC.refreshSymbolTable()
    }

    /*------------------- Chr use tests -------------------*/

    it should "Static analysis: Chr use negative" in {
        sC.semanticCheck(stmtParse("char c = chr -2").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Chr use negative"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Chr use negative expression" in {
        sC.semanticCheck(stmtParse("char c = chr (5 - 9)").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Chr use negative"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Chr use too big" in {
        sC.semanticCheck(stmtParse("char c = chr 128").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Chr use too big"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Chr use too big expression" in {
        sC.semanticCheck(stmtParse("char c = chr (102 + 77)").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Chr use too big"))
        sC.refreshSymbolTable()
    }

    /*------------------- Array indexing tests -------------------*/

    it should "Static analysis: Array index negative" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[-2]").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Negative array indexing"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Array index expression negative" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[3-7]").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Negative array indexing"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array access" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[5]").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Array index out of bounds"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array expression access" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; print a[2*2]").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Array index out of bounds"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array write" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; a[6] = 100").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Array index out of bounds"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Out of bounds array expression write" in {
        sC.semanticCheck(stmtParse("int[] a = [1, 2, 3]; a[6+7] = 100").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Array index out of bounds"))
        sC.refreshSymbolTable()
    }

    /*------------------- Integer overflow tests -------------------*/

    it should "Static analysis: Integer overflow" in {
        sC.semanticCheck(stmtParse("int x = 2147483647 + 1").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Integer overflow"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Integer underflow" in {
        sC.semanticCheck(stmtParse("int x = (-2147483648) - 1").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Integer overflow"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Integer multiplication overflow" in {
        sC.semanticCheck(stmtParse("int x = 2147483000 * 1000").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Integer overflow"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Integer negate overflow" in {
        sC.semanticCheck(stmtParse("int x = -(-2147483648)").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Integer overflow"))
        sC.refreshSymbolTable()
    }

    /*------------------- Null dereference tests -------------------*/

    it should "Static analysis: Free null" in {
        sC.semanticCheck(stmtParse("pair(int, int) a = null; free a").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Cannot free null pair"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Read null" in {
        sC.semanticCheck(stmtParse("pair(int, int) a = null; read fst a").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Cannot use null pair"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Set null" in {
        sC.semanticCheck(stmtParse("pair(int, int) a = null; fst a = 1").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Cannot use null pair"))
        sC.refreshSymbolTable()
    }

    it should "Static analysis: Use null" in {
        sC.semanticCheck(stmtParse("pair(int, int) a = null; int x = snd a").getOrElse(Skip))
        sC.getSemanticErrors shouldBe List(SemanticError("Static analysis error: Cannot use null pair"))
        sC.refreshSymbolTable()
    }
}
