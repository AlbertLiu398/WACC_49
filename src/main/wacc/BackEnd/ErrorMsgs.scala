package wacc

// Stores all errorMsg that could pop up 
object ErrorMsgs {
    final val ERR_OUT_OF_MEMORY_MSG = "fatal error: out of memory"
    final val ERR_OUT_OF_BOUND_MSG = s"fatal error: array index out of bounds"
    final val ERR_DIV_ZERO_MSG = "DivideByZeroError: divide or modulo by zero"
    final val ERR_NULL_MSG = "fatal error: null pair dereferenced or freed"
    final val ERR_OVERFLOW_MSG = "OverflowError: the result is too small/large to store in a 4-byte signed-integer"
    final val ERR_BAD_CHAR_MSG = "fatal error: char is not ascii character 0-127" 
    
}