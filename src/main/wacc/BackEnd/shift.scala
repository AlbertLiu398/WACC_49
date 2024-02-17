package wacc


object shift {
    sealed trait Shift
    case class LSL(value: Int) extends Shift
    case class LSR(value: Int) extends Shift
    case class ASR(value: Int) extends Shift
    case class ROR(value: Int) extends Shift
}

