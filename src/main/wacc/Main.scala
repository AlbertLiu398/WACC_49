package wacc

import parsley.{Failure, Success}
import scala.io.Source
import parser._
import FileConverter._
import java.io._

object Main {
    final val semanticError = 200
    final val syntaxError = 100
    final val optimisationFlag = "-o"

    def main(args: Array[String]): Unit = {

        try {
            val optimise = args.contains(optimisationFlag)
        
            args.headOption match {
                case Some(filePath) => 
                  {
                    val fileContents : String =  {
                        Source.fromFile(filePath).mkString 
                    }
                    Source.fromFile(filePath).close()
                    val result = parser.parse(fileContents)
                    

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
                            FileConverter.convertToAssembly(filePath, prog, sT.getVarList(), optimise)
                            println(s"Assembly file path: $filePath")
                            
                 
                        case Failure(msg) => 
                            println(msg)
                            sys.exit(syntaxError)
                        }                    
                    } 
                case None => println("please enter a file name")
            }

        } catch {
            case _: Throwable => throw new Exception("Invalid file path")
        }
    }
}
