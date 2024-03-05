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




  class generateInstrTest extends AnyFlatSpec with Matchers {

//     private val sT = new SymbolTable
//     val varList: mutable.ListBuffer[Int] = mutable.ListBuffer().append(1)
//     private val codeGenerator = new CodeGenerator(varList.toList)

//     def refreshAndGenerate(ast: ASTNode): List[Instruction] = {
//       codeGenerator.refreshInstructions()
//       codeGenerator.generateInstructions(ast)
//       codeGenerator.getInstructions()
//     }

//     it should "generate Add instruction" in {
//       val ast = Add(IntLiter(1), IntLiter(1))
//       refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 32), Reg(8, 32)), I_Move(Reg(8, 64), ImmVal(1)), I_Adds(Reg(8, 32), Reg(9, 32), Reg(8, 32)), I_Branch(I_Label("_errOverflow"), VS), I_Sxtw(Reg(8, 64), Reg(8, 32)))
//       codeGenerator.revertTempRegs()
//     }

//     it should "generate Sub instruction easy" in {
//       val ast = Sub(IntLiter(1), IntLiter(2))
//       refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 32), Reg(8, 32)), I_Move(Reg(8, 64), ImmVal(2)), I_Subs(Reg(8, 32), Reg(9, 32), Reg(8, 32)), I_Branch(I_Label("_errOverflow"), VS), I_Sxtw(Reg(8, 64), Reg(8, 32)))
//       codeGenerator.revertTempRegs()
//     }
  

//     it should "generate Sub instruction" in {
//        val ast = Sub(IntLiter(1), Sub(IntLiter(2), IntLiter(1)))
//       refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 64), Reg(8, 64)), I_Move(Reg(8, 64), ImmVal(2)), I_Move(Reg(10, 32), Reg(8, 32)), I_Move(Reg(8, 64), ImmVal(1)), I_Subs(Reg(8, 32), Reg(10, 32), Reg(8, 32)), I_Branch(I_Label("_errOverflow"), VS), I_Sxtw(Reg(8, 64), Reg(8, 32)), I_Subs(Reg(8, 32), Reg(9, 32), Reg(8, 32)), I_Branch(I_Label("_errOverflow"), VS), I_Sxtw(Reg(8, 64), Reg(8, 32)))
//       codeGenerator.revertTempRegs()
//     }

//     it should "generate Mul instruction" in {
//       val ast = Mul(IntLiter(1), IntLiter(2))
//       refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 64), Reg(8, 64)), I_Move(Reg(8, 64), ImmVal(2)), I_SMul(Reg(8, 64), Reg(9, 32), Reg(8, 32)), I_Sbfx(Reg(9, 64), Reg(8, 64), ImmVal(31), ImmVal(1)), I_Cmp_Shift(Reg(9, 64), Reg(8, 64), ASR(32)), I_Branch(I_Label("_errOverflow"), NE))
//       codeGenerator.revertTempRegs()
//     }

//     it should "generate Div instruction" in {
//       val ast = Div(IntLiter(1), IntLiter(2))
//       refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 64), ImmVal(2)), I_Cbz(Reg(9, 64), I_Label("_errDivZero")), I_SDiv(Reg(8, 64), Reg(8, 64), Reg(9, 64)), I_Branch(I_Label("_errOverflow"), VS), I_Sxtw(Reg(8, 64), Reg(8, 32)))
//       codeGenerator.revertTempRegs()
//     }

//     it should "generate Mod instruction" in {
//       val ast = Mod(IntLiter(1), IntLiter(2))
//       refreshAndGenerate(ast) shouldBe List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 64), Reg(8, 64)), I_Move(Reg(8, 64), ImmVal(2)), I_Cbz(Reg(8, 64), I_Label("_errDivZero")), I_SDiv(Reg(10, 64), Reg(9, 64), Reg(8, 64)), I_Mul(Reg(10, 64), Reg(10, 64), Reg(8, 64)), I_Sub(Reg(8, 64), Reg(9, 64), Reg(10, 64), false), I_Branch(I_Label("_errOverflow"), VS), I_Sxtw(Reg(8, 64), Reg(8, 32)))
//       codeGenerator.revertTempRegs()
//     }

//     it should "generate LessThan instruction" in {
//       val ast = LessThan(IntLiter(1), IntLiter(2))
//       refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8, 64), ImmVal(1)), I_Move(Reg(9, 64), Reg(8, 64)), I_Move(Reg(8, 64), ImmVal(2)), I_Cmp(Reg(9, 64), Reg(8, 64)), I_CSet(Reg(8, 64), LT))
//       codeGenerator.revertTempRegs()
//     }

//   it should "generate And instruction" in {
//     val ast = And(IntLiter(1), IntLiter(2))
//     refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8, 64), ImmVal(1)), I_Cmp(Reg(8, 64), ImmVal(1)), I_Branch(I_Label(".L0"), NE), I_Move(Reg(8, 64), ImmVal(2)), I_Cmp(Reg(8, 64), ImmVal(1)), I_Label(".L0"), I_CSet(Reg(8, 64), EQ), I_StorePair(Reg(8, 64), zeroReg(0), Content(spReg(31), ImmVal(-16), LSL(0)), ImmVal(0), true), I_LoadPair(Reg(8, 64), zeroReg(0), Content(spReg(31), ImmVal(0), LSL(0)), ImmVal(16), false), I_Move(Reg(8, 64), Reg(8, 64)))
//     codeGenerator.revertTempRegs()
//   }
//   it should "generate Or instruction" in {
//     val ast = Or(IntLiter(1), IntLiter(2))
//     refreshAndGenerate(ast) shouldBe   List(I_Move(Reg(8, 64), ImmVal(1)), I_Cmp(Reg(8, 64), ImmVal(1)), I_Branch(I_Label(".L1"), EQ), I_Move(Reg(8, 64), ImmVal(2)), I_Cmp(Reg(8, 64), ImmVal(1)), I_Label(".L1"), I_CSet(Reg(8, 64), EQ), I_StorePair(Reg(8, 64), zeroReg(0), Content(spReg(31), ImmVal(-16), LSL(0)), ImmVal(0), true), I_LoadPair(Reg(8, 64), zeroReg(0), Content(spReg(31), ImmVal(0), LSL(0)), ImmVal(16), false), I_Move(Reg(8, 64), Reg(8, 64)))
//    codeGenerator.revertTempRegs()
//   }
//   it should "generate Not instruction" in {
//     val ast = Invert(BoolLiter(true))
//     refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8,X_REGISTER_SIZE), ImmVal(1)), I_Cmp(x8, ImmVal(1)),I_CSet(x8, NE), I_StorePair(x8, xzr, Content(sp, ImmVal(-16)),ImmVal(0), true), 
//                                                     I_LoadPair(x8, xzr, Content(sp), ImmVal(16)), I_Move(x8, x8))
//     codeGenerator.revertTempRegs()
//   }

//   it should "generate Neg instruction" in {
//     val ast = Negate(IntLiter(1))
//     refreshAndGenerate(ast) shouldBe  List(I_Move(Reg(8,X_REGISTER_SIZE), ImmVal(-1)))
//     codeGenerator.revertTempRegs()
//   }

// }
  }



