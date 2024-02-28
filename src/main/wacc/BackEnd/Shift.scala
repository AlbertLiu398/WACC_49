package wacc


object Shift {
    sealed trait Shifts {
        def getValue() : String
    }
    case class LSL(value: Int) extends Shifts {
        override def getValue(): String = s"LSL #$value"
    }
    case class LSR(value: Int) extends Shifts {
        override def getValue(): String = s"LSR #$value"
    }
    case class ASR(value: Int) extends Shifts {
        override def getValue(): String = s"ASR #$value"
    }
    case class ROR(value: Int) extends Shifts {
        override def getValue(): String = s"ROR #$value"
    }
}