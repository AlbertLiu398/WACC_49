package wacc 

import instruction._
object Constant { 
    
    //  ------------- Regsiters (x0 - x7)  argument registers -------------
    val x0 = Reg(0)
    val x1 = Reg(1)
    val x2 = Reg(2)
    val x3 = Reg(3)
    val x4 = Reg(4)
    val x5 = Reg(5)
    val x6 = Reg(6)
    val x7 = Reg(7)
    
   // ------------- Register (x8/xr) indirect result register -------------
    val x8 = Reg(8)
    
   // ------------- Register (x9-x15) general-purpose -------------
    val x9 = Reg(9)
    val x10 = Reg(10)
    val x11 = Reg(11)
    val x12 = Reg(12)
    val x13 = Reg(13)
    val x14 = Reg(14)
    val x15 = Reg(15)
    

   // ------------- Register (x16-x17) intra-procedure-call temporary registers -------------
    val x16 = Reg(16)
    val x17 = Reg(17)


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

   // ------------- Register (x29) frame pointer -------------
    val x29 = Reg(29)
    
   // ------------- Register (x30) link register -------------
    val x30 = Reg(30)

    // ------------- Register (x31) stack pointer -------------
    val x31 = spReg(31)
    
   
    

    
}
