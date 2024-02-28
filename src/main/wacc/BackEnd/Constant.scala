package wacc 

import Instruction._
import scala.collection._
object Constant { 

    // Store size

    final val EOF: String = "\n"

    final val BYTE_SIZE: Int = 1
    final val WORD_SIZE: Int = 4
    final val CHAR_SIZE: Int = 1
    final val BOOL_SIZE: Int = 1
    final val INT_SIZE: Int = 4
    final val STRING_SIZE: Int = 8
    final val PAIR_TYPE_SIZE: Int = 8
    final val EMPTY_SIZE: Int = 0
    final val ARRAY_TYPE_SIZE: Int = 16
    final val PAIR_SIZE: Int = 16

    final val ARRAY_ELEM_SIZE: Int = 4
    

    
    
    //  ------------- Registers (x0 - x7)  argument registers -------------

    // Register x0 is both an argument and an return result register
    final val x0 = Reg(0)
    final val x1 = Reg(1)
    final val x2 = Reg(2)
    final val x3 = Reg(3)
    final val x4 = Reg(4)
    final val x5 = Reg(5)
    final val x6 = Reg(6)
    final val x7 = Reg(7)
    var unused_ParamRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x0, x1, x2, x3, x4, x5, x6, x7)
    var used_ParamRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    
    
   // ------------- Register (x8/xr) indirect result register -------------
    final val x8 = Reg(8)
    
   // ------------- Register (x9-x15) general-purpose (-------------
    final val x9 = Reg(9)
    final val x10 = Reg(10)
    final val x11 = Reg(11)
    final val x12 = Reg(12)
    final val x13 = Reg(13)
    final val x14 = Reg(14)
    final val x15 = Reg(15)
    var used_TempRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    var unused_TempRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x9, x10, x11, x12, x13, x14, x15)
    final val unused_TempRegs_copy = List(x9, x10, x11, x12, x13, x14, x15)

   // ------------- Register (x16-x17) intra-procedure-call temporary registers -------------
    final val x16 = Reg(16)
    final val x17 = Reg(17)
    var used_IntraRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    var unused_IntraRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x16, x17)


   // ------------- Register (x18) platform register -------------
    final val x18 = Reg(18)
    
   // ------------- Register (x19-x28) general-purpose -------------
    final val x19 = Reg(19)
    final val x20 = Reg(20)
    final val x21 = Reg(21)
    final val x22 = Reg(22)
    final val x23 = Reg(23)
    final val x24 = Reg(24)
    final val x25 = Reg(25)
    final val x26 = Reg(26)
    final val x27 = Reg(27)
    final val x28 = Reg(28)
    var used_ResultRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    var unused_ResultRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x19, x20, x21, x22, x23, x24, x25, x26, x27, x28)

   // ------------- Register (x29) frame pointer -------------
    final val fp = Reg(29)
    
   // ------------- Register (x30) link register -------------
    final val lr = Reg(30)

    // ------------- Register (x31) stack pointer -------------
    final val sp = spReg(31)

    final val xzr = zeroReg(0)
    
}
