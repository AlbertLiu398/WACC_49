package wacc 

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}


object FileConverter {

  def generateAssemblyCode(prog : ast.ASTNode, list : List[Int]): String = {
    val codeGenerator = new CodeGenerator(list)
    val instrus = codeGenerator.generateInstructions(prog)
    var result = ""
    for (instr <- codeGenerator.getInstructions()) {
      result += instr.printInstr() 
    }
    result
  }

  def convertToAssembly(filePath: String, prog : ast.ASTNode, list : List[Int]): Unit = {
    val waccFile = new File(filePath)
    
    if (waccFile.exists() && waccFile.isFile && filePath.endsWith(".wacc")) {
      val asmFileName = waccFile.getName.replaceAll("\\.wacc$", ".s")
      val asmFile = new File(asmFileName)
      
      if (asmFile.createNewFile()) {

        //  generate assembly code and write to asm file
        val writer = new BufferedWriter(new FileWriter(asmFile))
        
        writer.write(generateAssemblyCode(prog, list))
        
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
