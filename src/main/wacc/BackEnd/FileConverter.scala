package wacc 

import java.io._
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

  def convertToAssembly(filePath: String, prog: ast.ASTNode, list: List[Int]): Unit = {
    val waccFile = new File(filePath)

    if (waccFile.exists() && waccFile.isFile && filePath.endsWith(".wacc")) {
      val asmFileName = waccFile.getName.replaceAll("\\.wacc$", ".s")
      val asmFile = new File(asmFileName)

      try {
        val fos = new FileOutputStream(asmFile, false)
        val writer = new BufferedWriter(new OutputStreamWriter(fos))

        // generate assembly code and write to asm file
        writer.write(generateAssemblyCode(prog, list))

        writer.close()

        println(s"Assembly file created/overwritten: $asmFileName")
      } catch {
        case e: IOException =>
          println(s"Error creating/overwriting assembly file: ${e.getMessage}")
      }
    } else {
      println("Invalid or non-existent WACC file.")
    }
  }

}
