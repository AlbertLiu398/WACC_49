package wacc


object shift {
    sealed trait Shift {
        def getValue() : String
    }
    case class LSL(value: Int) extends Shift {
        override def getValue(): String = s"LSL #$value"
    }
    case class LSR(value: Int) extends Shift {
        override def getValue(): String = s"LSR #$value"
    }
    case class ASR(value: Int) extends Shift {
        override def getValue(): String = s"ASR #$value"
    }
    case class ROR(value: Int) extends Shift {
        override def getValue(): String = s"ROR #$value"
    }
}