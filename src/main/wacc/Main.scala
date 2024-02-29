package wacc

import parsley.{Failure, Success}
import scala.io.Source
import parser._
import FileConverter._
import scala.util.{Try}
import java.io.{File, PrintWriter}

object Main {
    final val semanticError = 200
    final val syntaxError = 100
    def main(args: Array[String]): Unit = {
       
        args.headOption match {
            case Some(filePath) => 
              {
                val fileContents : Try[String] = Try {
                    Source.fromFile(filePath).mkString 
                }
                Source.fromFile(filePath).close()
                val result = parser.parse(fileContents.get)
                

                result match {
                    case Success(prog) => 
                        println(s"$prog")
                        val sT = new SymbolTable
                        val semanticchecker = new semanticsChecker(sT)
                        semanticchecker.semanticCheck(prog)
                        val errors = semanticchecker.getSemanticErrors
                        if (errors.nonEmpty) {
                            errors.foreach(println)
                            sys.exit(semanticError)
                        }

                        /* 1. create assembly file 
                           2. generate assembly code and write to asm file */
                        FileConverter.convertToAssembly(filePath, prog, sT.getVarList())
                        println(s"Assembly file path: $filePath")
                        println("Generated Assembly Code:")
                        println(FileConverter.generateAssemblyCode(prog, sT.getVarList()))
                        
             
                    case Failure(msg) => 
                        println(msg)
                        sys.exit(syntaxError)
                    }
                } 
            case None => println("please enter a file name")
        }
    }
}
