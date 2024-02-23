package wacc

import ast._
import scala.language.implicitConversions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CodeGenerator._
import Constant._
import instruction._
import conditions._



  class generateInstrTest extends AnyFlatSpec with Matchers {
    it should "generate Add instruction" in {
      val ast = Add(IntLiter(1), IntLiter(1))
      refreshInstructions()
      CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe List(I_Move(x8, ImmVal(1)), I_Add(x8, x8, ImmVal(1)))
      revertTempRegs()
    }

    it should "generate Sub instruction easy" in {
      val ast = Sub(IntLiter(1), IntLiter(2))
      refreshInstructions()
      CodeGenerator.generateInstructions(ast) 
      CodeGenerator.getInstructions() shouldBe List(I_Move(x8, ImmVal(1)), I_Sub(x8, x8, ImmVal(2),false))
      revertTempRegs()
    }
  

    it should "generate Sub instruction" in {
       val ast = Sub(IntLiter(1), Sub(IntLiter(2), IntLiter(1)))
      refreshInstructions()
      CodeGenerator.generateInstructions(ast) 
      CodeGenerator.getInstructions() shouldBe List(I_Move(Reg(8),ImmVal(1)), I_Move(Reg(9),Reg(8)), I_Move(Reg(8),ImmVal(2)), I_Sub(Reg(8),Reg(8),ImmVal(1),false), I_Sub(Reg(8),Reg(9),Reg(8),false))
      revertTempRegs()
    }

    it should "generate Mul instruction" in {

      val ast = Mul(IntLiter(1), IntLiter(2))
      refreshInstructions()
      CodeGenerator.generateInstructions(ast) 
      CodeGenerator.getInstructions() shouldBe List(I_Move(Reg(8),ImmVal(1)), I_Move(Reg(9),Reg(8)), I_Move(Reg(8),ImmVal(2)), I_Mul(Reg(8),Reg(9),Reg(8)))
      revertTempRegs()
    }

    it should "generate Div instruction" in {
      val ast = Div(IntLiter(1), IntLiter(2))
      refreshInstructions()
      val result = CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe List(I_Move(x8, ImmVal(1)), I_UDiv(x8, x8, ImmVal(2)))
      revertTempRegs()
    }

    it should "generate Mod instruction" in {
      val ast = Mod(IntLiter(1), IntLiter(2))
      refreshInstructions()
      val result = CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_UDiv(Reg(10), Reg(9), Reg(8)), I_Mul(Reg(10), Reg(10), Reg(8)), I_Sub(Reg(8), Reg(9), Reg(10), false))
      revertTempRegs()
    }

    it should "generate LessThan instruction" in {
      val ast = LessThan(IntLiter(1), IntLiter(2))
      refreshInstructions()
      val result = CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), LT)) 
      revertTempRegs()
    }

    it should "generate LessThanEq instruction" in {
      val ast = LessThanEq(IntLiter(1), IntLiter(2))
      refreshInstructions()
      val result = CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), LE)) 
      revertTempRegs()
    }
  
  it should "generate GreaterThan instruction" in {
    val ast = GreaterThan(IntLiter(1), IntLiter(2))
    refreshInstructions()
    val result = CodeGenerator.generateInstructions(ast)
    CodeGenerator.getInstructions() shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), GT))
    revertTempRegs()
  }
  it should "generate GreaterThanEq instruction" in {
    val ast = GreaterThanEq(IntLiter(1), IntLiter(2))
    refreshInstructions()
    val result = CodeGenerator.generateInstructions(ast)
    CodeGenerator.getInstructions() shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), GE))
    revertTempRegs()
}

  it should "generate Eq instruction" in {
    val ast = Eq(IntLiter(1), IntLiter(2))
      refreshInstructions()
      val result = CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), EQ))
      revertTempRegs()
  }

  it should "generate NotEq instruction" in {
    val ast = NotEq(IntLiter(1), IntLiter(2))
      refreshInstructions()
      val result = CodeGenerator.generateInstructions(ast)
      CodeGenerator.getInstructions() shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), NE))
      revertTempRegs()
  }
  // it should "generate And instruction" in {
  //   val ast = And(IntLiter(1), IntLiter(1))
  //   refreshInstructions()
  //   val result = CodeGenerator.generateInstructions(ast)
  //   // result shouldBe Success(mutable.ListBuffer(I_Move(x8, ImmVal(1)), I_And(x8, x8, ImmVal(1))))
  // }
  // it should "generate Or instruction" in {
  //   val ast = Or(IntLiter(1), IntLiter(1))
  //   refreshInstructions()
  //   val result = CodeGenerator.generateInstructions(ast)
  //   // result shouldBe Success(mutable.ListBuffer(I_Move(x8, ImmVal(1)), I_Or(x8, x8, ImmVal(1))))
  // }
  // it should "generate Not instruction" in {
  //   val ast = Not(IntLiter(1))
  //   refreshInstructions()
  //   val result = CodeGenerator.generateInstructions(ast)
  //   // result shouldBe Success(mutable.ListBuffer(I_Not(x8, ImmVal(1))))
  // }
  // it should "generate Neg instruction" in {
  //   val ast = Neg(IntLiter(1))
  //   refreshInstructions()
  //   val result = CodeGenerator.generateInstructions(ast)
  //   // result shouldBe Success(mutable.ListBuffer(I_Neg(x8, ImmVal(1))))
  // }
  // it should "generate IntLiter instruction" in {
  //   val ast = IntLiter(1)
  //   refreshInstructions()
  //   val result = CodeGenerator.generateInstructions(ast)
  //   // result shouldBe Success(mutable.ListBuffer(I_Move(x8, ImmVal(1)))
  // }
  // it should "generate BoolLiter instruction" in {
  //   val ast = BoolLiter(true)
  //   refreshInstructions()
  //   val result = CodeGenerator.generateInstructions(ast)
  //   // result shouldBe Success(mutable.ListBuffer(I_Move(x8, ImmVal(1)))
  // }


}



