package wacc


object Shift {
    sealed trait Shifts {
        def getValue() : String
    }
    case class LSL(value: Int) extends Shifts {
        override def getValue(): String = s"lsl #$value"
    }
    case class LSR(value: Int) extends Shifts {
        override def getValue(): String = s"lsr #$value"
    }
    case class ASR(value: Int) extends Shifts {
        override def getValue(): String = s"asr #$value"
    }
    case class ROR(value: Int) extends Shifts {
        override def getValue(): String = s"ror #$value"
    }
}