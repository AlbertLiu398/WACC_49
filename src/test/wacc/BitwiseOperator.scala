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

class BitwiseOperator extends AnyFlatSpec with Matchers {
    it should "parse BitwiseOperator" in {
        exprParse("1 & 2") shouldBe Success(BitAnd(ByteIntLiter(1), ByteIntLiter(2)))
        exprParse("1 | 2") shouldBe Success(BitOr(ByteIntLiter(1), ByteIntLiter(2)))
        exprParse("~2") shouldBe Success(BitNot(ByteIntLiter(2)))
    }
}