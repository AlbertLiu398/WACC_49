package wacc

 // Conditions needed to handle in the cmp 
object Conditions extends Enumeration {
  type Conditions = Value
  val EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, LT, GT, LE, GE = Value
  val AL: Value = Value("")
}
