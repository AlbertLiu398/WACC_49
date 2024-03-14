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

class Byte_shortTest extends AnyFlatSpec with Matchers {
  it should "parse byte int" in {
    allTypeParse("int_byte_8") shouldBe Success(BaseType("int_byte_8"))
  }

  it should "parse short int" in {
    allTypeParse("int_short") shouldBe Success(BaseType("int_short"))
  }

  it should "parse int expr" in {
    allTypeParse("int") shouldBe Success(BaseType("int"))
    stmtParse("int_byte_8 x = 1;int y = 2 + x") shouldBe Success(SeqStmt(NewAssignment(BaseType("int_byte_8"), Ident("x"), ByteIntLiter(1)), NewAssignment(BaseType("int"), Ident("y"), Add(ByteIntLiter(2), Ident("x")))))
    stmtParse("int_short x = 1;int y = 2 + x") shouldBe Success(SeqStmt(NewAssignment(BaseType("int_short"), Ident("x"), ByteIntLiter(1)), NewAssignment(BaseType("int"), Ident("y"), Add(ByteIntLiter(2), Ident("x")))))
    }
}
