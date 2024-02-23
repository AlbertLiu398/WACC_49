package wacc 

import instruction._
import scala.collection._
object Constant { 
    
    //  ------------- Registers (x0 - x7)  argument registers -------------

    // Register x0 is both an argument and an return result register
    val x0 = Reg(0)
    val x1 = Reg(1)
    val x2 = Reg(2)
    val x3 = Reg(3)
    val x4 = Reg(4)
    val x5 = Reg(5)
    val x6 = Reg(6)
    val x7 = Reg(7)
    var unused_ParamRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x0, x1, x2, x3, x4, x5, x6, x7)
    var used_ParamRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    
    
   // ------------- Register (x8/xr) indirect result register -------------
    val x8 = Reg(8)
    
   // ------------- Register (x9-x15) general-purpose (-------------
    val x9 = Reg(9)
    val x10 = Reg(10)
    val x11 = Reg(11)
    val x12 = Reg(12)
    val x13 = Reg(13)
    val x14 = Reg(14)
    val x15 = Reg(15)
    var used_TempRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    var unused_TempRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x9, x10, x11, x12, x13, x14, x15)
    final val unused_TempRegs_copy = List(x9, x10, x11, x12, x13, x14, x15)

   // ------------- Register (x16-x17) intra-procedure-call temporary registers -------------
    val x16 = Reg(16)
    val x17 = Reg(17)
    var used_IntraRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    var unused_IntraRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x16, x17)


   // ------------- Register (x18) platform register -------------
    val x18 = Reg(18)
    
   // ------------- Register (x19-x28) general-purpose -------------
    val x19 = Reg(19)
    val x20 = Reg(20)
    val x21 = Reg(21)
    val x22 = Reg(22)
    val x23 = Reg(23)
    val x24 = Reg(24)
    val x25 = Reg(25)
    val x26 = Reg(26)
    val x27 = Reg(27)
    val x28 = Reg(28)
    var used_ResultRegs: mutable.ListBuffer[Register] = mutable.ListBuffer()
    var unused_ResultRegs: mutable.ListBuffer[Register] = mutable.ListBuffer(x19, x20, x21, x22, x23, x24, x25, x26, x27, x28)

   // ------------- Register (x29) frame pointer -------------
    val fp = Reg(29)
    
   // ------------- Register (x30) link register -------------
    val lr = Reg(30)

    // ------------- Register (x31) stack pointer -------------
    val sp = spReg(31)

    val xzr = zeroReg(0)
    
   
    

    
}
