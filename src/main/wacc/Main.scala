package wacc

import parsley.{Success, Failure}
import scala.io.Source
import parser._
object Main {
    def main(args: Array[String]): Unit = {
        println("hello WACC!")
        // println (allTypeParse("pair(pair(int, bool), pair(bool, int))"))

        args.headOption match {
            case Some(filePath) =>
                val fileContents: String = Source.fromFile(filePath).mkString
                Source.fromFile(filePath).close()

                val result = parser.parse(fileContents)

                result match {
                    case Success(prog) => 
                        println(s"$prog")
                        val semanticchecker = new semanticsChecker(new SymbolTable)
                        semanticchecker.semanticCheck(prog)
                        val errors = semanticchecker.getSemanticErrors
                        if (!errors.isEmpty) {
                            errors.foreach(println(_))
                            sys.exit(200)
                        }
                        
                    case Failure(msg) => 
                        println(msg)
                        sys.exit(100)
                        // println(msg)
                    }
            case None => println("please enter a file name")
        }
    }
}
