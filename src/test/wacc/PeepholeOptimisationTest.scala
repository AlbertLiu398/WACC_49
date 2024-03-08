package wacc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Instruction._
import Constant._
import PeepholeOptimisation._
import FileConverter._


class PeepholeOptimisationTest extends AnyFlatSpec with Matchers {

    // Helper function to create a 64-bit register for tesing purposes
    def XReg(n: Int): Register = Reg(n, X_REGISTER_SIZE)

    // ---------------Move---------------

    it should "optimize redundant move-move duplicate instructions" in {
        val instructions = List(
            I_Move(XReg(1), XReg(2)),
            I_Move(XReg(1), XReg(2))
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Move(XReg(1), XReg(2))
        )
    }
    
    it should "optimize redundant move-move (through x8) instructions" in {
        val instructions = List(
            I_Move(XReg(8), XReg(1)),
            I_Move(XReg(2), XReg(8))
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Move(XReg(2), XReg(1))
        )
    }

    it should "optimize redundant move-move (through x8, move immediate) instructions" in {
        val instructions = List(
            I_Move(XReg(8), ImmVal(1)),
            I_Move(XReg(2), XReg(8))
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Move(XReg(2), ImmVal(1))
        )
    }

    it should "optimize redundant move-move-move instructions" ignore {
        val instructions = List(
            I_Move(XReg(8), XReg(1)),
            I_Move(XReg(2), XReg(8)),
            I_Move(XReg(3), XReg(2))
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Move(XReg(3), XReg(1))
        )
    }

    it should "optimize redundant move-store (through x8) instructions" in {
        val instructions = List(
            I_Move(XReg(8), XReg(1)),
            I_Store(XReg(8), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Store(XReg(1), fp, ImmVal(28), false, false)
        )
    }

    it should "optimize redundant move-store (through x8, move immediate should not be modified) instructions" in {
        val instructions = List(
            I_Move(XReg(8), ImmVal(1)),
            I_Store(XReg(8), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Move(XReg(8), ImmVal(1)),
            I_Store(XReg(8), fp, ImmVal(28), false, false)
        )
    }

    it should "optimize redundant move-load instructions" in {
        val instructions = List(
            I_Move(XReg(0), XReg(1)),
            I_Load(XReg(0), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Load(XReg(0), fp, ImmVal(28), false, false)
        )
    }


    // ---------------Load---------------

    it should "optimize redundant load-load (different dst, same op/op2) instructions" in {
        val instructions = List(
            I_Load(XReg(1), fp, ImmVal(28), false, false),
            I_Load(XReg(0), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Load(XReg(1), fp, ImmVal(28), false, false),
            I_Move(XReg(0), XReg(1))
        )
    }

    it should "optimize redundant load-load (same dst, different op/op2) instructions" in {
        val instructions = List(
            I_Load(XReg(1), fp, ImmVal(28), false, false),
            I_Load(XReg(1), fp, ImmVal(32), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Load(XReg(1), fp, ImmVal(32), false, false)
        )
    }

    it should "optimize redundant load-move instructions" in {
        val instructions = List(
            I_Load(XReg(0), fp,  ImmVal(28), false, false),
            I_Move(XReg(0), XReg(1))
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Move(XReg(0), XReg(1))

        )
    }

    it should "optimize redundant load-store instructions" in {
        val instructions = List(
            I_Load(XReg(0), fp, ImmVal(28), false, false),
            I_Store(XReg(0), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Load(XReg(0), fp, ImmVal(28), false, false)
        )
    }

    // ---------------Store---------------

    it should "optimize redundant store-store instructions" in {
        val instructions = List(
            I_Store(XReg(0), fp, ImmVal(28), false, false),
            I_Store(XReg(0), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Store(XReg(0), fp, ImmVal(28), false, false)
        )
    }

    it should "optimize redundant store-load(different dst, same op/op2) instructions" in {
        val instructions = List(
            I_Store(XReg(0), fp, ImmVal(28), false, false),
            I_Load(XReg(1), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Store(XReg(0), fp, ImmVal(28), false, false),
            I_Move(XReg(1), XReg(0))
        )
    }
    
    it should "optimize redundant store-load(same dst, same op/op2) instructions" in {
        val instructions = List(
            I_Store(XReg(0), fp, ImmVal(28), false, false),
            I_Load(XReg(0), fp, ImmVal(28), false, false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List(
            I_Store(XReg(0), fp, ImmVal(28), false, false)

        )
    }

    // ---------------Stack---------------

    it should "optimize redundant push-pop instructions" in {
        val instructions = List(
            I_StorePair(XReg(8), xzr, Content(sp, ImmVal(-16)), ImmVal(0), true),
            I_LoadPair(XReg(8), xzr, Content(sp), ImmVal(16), false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List()
    }

    it should "optimize redundant pop-push instructions" in {
        val instructions = List(
            I_LoadPair(XReg(8), xzr, Content(sp), ImmVal(16), false),
            I_StorePair(XReg(8), xzr, Content(sp, ImmVal(-16)), ImmVal(0), true)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List()
    }

    it should "optimize redundant nested push-pop instructions" in {
        val instructions = List(
            I_LoadPair(XReg(1), xzr, Content(sp), ImmVal(-16), false),
            I_LoadPair(XReg(2), xzr, Content(sp), ImmVal(-16), false),
            I_StorePair(XReg(2), xzr, Content(sp, ImmVal(16)), ImmVal(0), true),
            I_StorePair(XReg(1), xzr, Content(sp, ImmVal(16)), ImmVal(0), true)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List()
    }

    it should "optimize redundant nested pop-push instructions" in {
        val instructions = List(
            I_StorePair(XReg(2), xzr, Content(sp, ImmVal(16)), ImmVal(0), true),
            I_StorePair(XReg(1), xzr, Content(sp, ImmVal(16)), ImmVal(0), true),
            I_LoadPair(XReg(1), xzr, Content(sp), ImmVal(-16), false),
            I_LoadPair(XReg(2), xzr, Content(sp), ImmVal(-16), false)
        )
        val optimizedInstructions = PeepholeOptimisation.runPeeopholeOptimisation(instructions)
        optimizedInstructions.toList shouldEqual List()
    }

    // Add more tests for other optimizations

}




