package wacc 

import java.io._
import java.nio.file.{Files, Paths, StandardCopyOption}
import PeepholeOptimisation._

  /* 1. create assembly file 
     2. generate assembly code and write to asm file */

object FileConverter {
  // genearte assembly code use the codeGenerator 


  def generateAssemblyCode(prog : ast.ASTNode, list : List[Int], optimise: Boolean): StringBuilder = {
    val codeGenerator = new CodeGenerator(list)
    val instrus = codeGenerator.generateInstructions(prog)
    var resultInstrus = codeGenerator.getInstructions()
    if (optimise) {
      resultInstrus = PeepholeOptimisation.runPeeopholeOptimisation(resultInstrus)
    } 
    ArmAssemblyWriter.translateProgram(resultInstrus)

  }

  def convertToAssembly(filePath: String, prog: ast.ASTNode, list: List[Int], optimise: Boolean): Unit = {
    val waccFile = new File(filePath)

    if (waccFile.exists() && waccFile.isFile && filePath.endsWith(".wacc")) {
      val asmFileName = waccFile.getName.replaceAll("\\.wacc$", ".s")
      val asmFile = new File(asmFileName)

      try {
        val fos = new FileOutputStream(asmFile, false)
        val writer = new BufferedWriter(new OutputStreamWriter(fos))

        // generate assembly code and write to asm file
        writer.write(generateAssemblyCode(prog, list, optimise).toString())

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

