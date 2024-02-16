package wacc

import parsley.{Failure, Success}
import scala.io.Source
import parser._
import java.io.File
object Main {
    final val semanticError = 200
    final val syntaxError = 100
    def main(args: Array[String]): Unit = {
       
        args.headOption match {
            case Some(filePath) => 
            // try
                {
                val fileContents: String = Source.fromFile(filePath).mkString 
                Source.fromFile(filePath).close()

                val result = parser.parse(fileContents)

                result match {
                    case Success(prog) => 
                        println(s"$prog")
                        val semanticchecker = new semanticsChecker(new SymbolTable)
                        semanticchecker.semanticCheck(prog)
                        val errors = semanticchecker.getSemanticErrors
                        if (errors.nonEmpty) {
                            errors.foreach(println)
                            sys.exit(semanticError)
                        }
                        
                    case Failure(msg) => 
                        println(msg)
                        sys.exit(syntaxError)
                    }
                } 
                // match {
                //     case Success(_) => 
                //     case Failure(e) => 
                //       println(e)
                //         sys.exit(syntaxError)
                //   }
            case None => println("please enter a file name")
        }
    }
}
