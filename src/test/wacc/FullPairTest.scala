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