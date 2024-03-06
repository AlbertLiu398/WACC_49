package wacc

import ast._
import scala.language.implicitConversions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Constant._
import Instruction._
import Conditions._
import org.scalatest._
import scala.collection._
import Shift._
import parser._
import parsley.{Failure, Result, Success}
import scala.io.Source

class FullPairTest extends AnyFlatSpec with Matchers {

    it should "parse nested pair unit test" in {
        allTypeParse("pair(int, pair)") shouldBe Success(PairType(BaseType("int"), PairTypeElem))
        allTypeParse("pair(int, pair(int, int))") shouldBe Success(PairType(BaseType("int"), PairType(BaseType("int"), BaseType("int"))))
        allTypeParse("pair(pair(int, int), pair(int, int))") shouldBe Success(PairType(PairType(BaseType("int"), BaseType("int")), PairType(BaseType("int"), BaseType("int"))))
    }

    it should "parse nested pair integration test" in {
        parseFile("pairs/nestedPair.wacc") shouldBe Success(Program(List(), SeqStmt(NewAssignment(PairType(BaseType("int"), BaseType("int")), Ident("p"), NewPairRValue(IntLiter(2), IntLiter(3))), NewAssignment(PairType(BaseType("int"), PairTypeElem), Ident("q"), NewPairRValue(IntLiter(1), Ident("p"))))))
        parseFile("pairs/nestedPairLeftAssign.wacc") shouldBe Success(Program(List(), SeqStmt(NewAssignment(PairType(BaseType("int"), BaseType("int")), Ident("p"), NewPairRValue(IntLiter(2), IntLiter(3))), SeqStmt(NewAssignment(PairType(BaseType("int"), PairTypeElem), Ident("q"), NewPairRValue(IntLiter(1), Ident("p"))), SeqStmt(Assignment(FstPairElem(SndPairElem(Ident("q"))), IntLiter(7)), SeqStmt(NewAssignment(BaseType("int"), Ident("x"), FstPairElem(Ident("p"))), Print(Ident("x"), true)))))))
        parseFile("pairs/nestedPairRightExtract.wacc") shouldBe Success(Program(List(), SeqStmt(NewAssignment(PairType(BaseType("int"), BaseType("int")), Ident("p"), NewPairRValue(IntLiter(2), IntLiter(3))), SeqStmt(NewAssignment(PairType(BaseType("int"), PairTypeElem), Ident("q"), NewPairRValue(IntLiter(1), Ident("p"))), SeqStmt(NewAssignment(BaseType("int"), Ident("x"), FstPairElem(SndPairElem(Ident("q")))), Print(Ident("x"), true))))))
    }


    def parseFile(filePath: String): Result[String,Program] = {
        val semanticError = 200
        val syntaxError = 100
        val fileContents : String =  {
            Source.fromFile(filePath).mkString 
        }
        Source.fromFile(filePath).close()
        val result = parser.parse(fileContents)

        result
    }

}