package wacc

import scala.collection.mutable
import Instruction._


trait AssemblyWriter {
  def translateProgram(instrus: List[Instruction]): StringBuilder ={
    val result = new StringBuilder()
    for (instr <- instrus) {
      result.append(translateInstruction(instr))
    }
    result
  }

  def translateInstruction(instr: Instruction): String 


}