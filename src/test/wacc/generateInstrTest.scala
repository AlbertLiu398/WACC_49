package wacc

import ast._
import scala.language.implicitConversions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Constant._
import Instruction._
import Conditions._
import org.scalatest._




  class generateInstrTest extends AnyFlatSpec with Matchers {

    private val sT = new SymbolTable
    private val codeGenerator = new CodeGenerator(sT.getVarList())

    def refreshAndGenerate(ast: ASTNode): List[Instruction] = {
      codeGenerator.refreshInstructions()
      codeGenerator.generateInstructions(ast)
      codeGenerator.getInstructions()
    }

    it should "generate Add instruction" in {
      val ast = Add(IntLiter(1), IntLiter(1))
      refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Add(Reg(8), Reg(8), ImmVal(1)), I_Branch(I_Label("_errOverflow"), VS))
      codeGenerator.revertTempRegs()
    }

    it should "generate Sub instruction easy" in {
      val ast = Sub(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(x8, ImmVal(1)), I_Sub(x8, x8, ImmVal(2),false), I_Branch(I_Label("_errOverflow"), VS))
      codeGenerator.revertTempRegs()
    }
  

    it should "generate Sub instruction" in {
       val ast = Sub(IntLiter(1), Sub(IntLiter(2), IntLiter(1)))
      refreshAndGenerate(ast) shouldBe   List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Sub(Reg(8), Reg(8), ImmVal(1), false), I_Branch(I_Label("_errOverflow"), VS), I_Sub(Reg(8), Reg(9), Reg(8), false), I_Branch(I_Label("_errOverflow"), VS))
      codeGenerator.revertTempRegs()
    }

    it should "generate Mul instruction" in {
      val ast = Mul(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8),ImmVal(1)), I_Move(Reg(9),Reg(8)), I_Move(Reg(8),ImmVal(2)), I_Mul(Reg(8),Reg(9),Reg(8)), I_Branch(I_Label("_errOverflow"), VS))
      codeGenerator.revertTempRegs()
    }

    it should "generate Div instruction" in {
      val ast = Div(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(x8, ImmVal(1)), I_UDiv(x8, x8, ImmVal(2)))
     codeGenerator.revertTempRegs()
    }

    it should "generate Mod instruction" in {
      val ast = Mod(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cbz(Reg(9), I_Label("_errDivZero")), I_UDiv(Reg(10), Reg(9), Reg(8)), I_Mul(Reg(10), Reg(10), Reg(8)), I_Sub(Reg(8), Reg(9), Reg(10), false), I_Branch(I_Label("_errOverflow"), VS)) 
      codeGenerator.revertTempRegs()
    }

    it should "generate LessThan instruction" in {
      val ast = LessThan(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(10), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(10), Reg(8)), I_CSet(Reg(8), LT))
      codeGenerator.revertTempRegs()
    }

    it should "generate LessThanEq instruction" in {
      val ast = LessThanEq(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), LT))
      codeGenerator.revertTempRegs()
    }
  
  it should "generate GreaterThan instruction" in {
    val ast = GreaterThan(IntLiter(1), IntLiter(2))
    refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(12), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(12), Reg(8)), I_CSet(Reg(8), GT))
    codeGenerator.revertTempRegs()
  }

  it should "generate GreaterThanEq instruction" in {
    val ast = GreaterThanEq(IntLiter(1), IntLiter(2))
    refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(12), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(12), Reg(8)), I_CSet(Reg(8), GE))
   codeGenerator.revertTempRegs()
}

  it should "generate Eq instruction" in {
    val ast = Eq(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(13), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(13), Reg(8)), I_CSet(Reg(8), EQ))
      codeGenerator.revertTempRegs()
  }

  it should "generate NotEq instruction" in {
    val ast = NotEq(IntLiter(1), IntLiter(2))
      refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8), ImmVal(1)), I_Move(Reg(9), Reg(8)), I_Move(Reg(8), ImmVal(2)), I_Cmp(Reg(9), Reg(8)), I_CSet(Reg(8), NE))
      codeGenerator.revertTempRegs()
  }
  it should "generate And instruction" in {
    val ast = And(IntLiter(1), IntLiter(2))
    refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_And(x8, x8, ImmVal(2)))
    codeGenerator.revertTempRegs()
  }
  it should "generate Or instruction" in {
    val ast = Or(IntLiter(1), IntLiter(2))
    refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Orr(x8, x8, ImmVal(2)))
   codeGenerator.revertTempRegs()
  }
  it should "generate Not instruction" in {
    val ast = Invert(BoolLiter(true))
    refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8), ImmVal(1)), I_Cmp(x8, ImmVal(1)),I_CSet(x8, NE), I_StorePair(x8, xzr, Content(sp, ImmVal(-16)),ImmVal(0), true), 
                                                    I_LoadPair(x8, xzr, Content(sp), ImmVal(16)), I_Move(x8, x8))
    codeGenerator.revertTempRegs()
  }

  it should "generate Neg instruction" in {
    val ast = Negate(IntLiter(1))
    refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8), ImmVal(-1)))
    codeGenerator.revertTempRegs()
  }


}



