package wacc 

import java.io.{File, PrintWriter}
import CodeGenerator._

object FileConverter {

  def convertToAssembly(filePath: String, prog : ast.ASTNode): Unit = {
    val waccFile = new File(filePath)
    
    if (waccFile.exists() && waccFile.isFile && filePath.endsWith(".wacc")) {
      val asmFileName = waccFile.getName.replaceAll("\\.wacc$", ".s")
      val asmFile = new File(asmFileName)
      
      if (asmFile.createNewFile()) {

        //  generate assembly code and write to asm file
        val writer = new PrintWriter(asmFile)
        val instrus = CodeGenerator.generateInstructions(prog)
        for (instr <- CodeGenerator.getInstructions()) {
          writer.println(instr.printInstr())
        }
        writer.close()
        
        println(s"Assembly file created: $asmFileName")
      } else {
        println("Error creating assembly file.")
      }
    } else {
      println("Invalid or non-existent WACC file.")
    }
  }
}
