package wacc 

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}


object FileConverter {

  def convertToAssembly(filePath: String, prog : ast.ASTNode, list : List[Int]): Unit = {
    val waccFile = new File(filePath)
    
    if (waccFile.exists() && waccFile.isFile && filePath.endsWith(".wacc")) {
      val asmFileName = waccFile.getName.replaceAll("\\.wacc$", ".s")
      val asmFile = new File(asmFileName)
      val codeGenerator = new CodeGenerator(list)
      
      if (asmFile.createNewFile()) {

        //  generate assembly code and write to asm file
        val writer = new BufferedWriter(new FileWriter(asmFile))
        val instrus = codeGenerator.generateInstructions(prog)
        
        for (instr <- codeGenerator.getInstructions()) {
          writer.write(instr.printInstr().mkString("\n"))
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
