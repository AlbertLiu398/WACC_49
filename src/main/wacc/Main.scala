package wacc

import parsley.{Success, Failure}
import scala.io.Source
object Main {
    def main(args: Array[String]): Unit = {

        args.headOption match {
            case Some(filePath) =>
                val fileContents: String = Source.fromFile(filePath).mkString
                val syntaxContents: String = Source.fromFile("./syntax1.txt").mkString
                val semanticContents: String = Source.fromFile("./semantic1.txt").mkString
                println(fileContents)
                Source.fromFile(filePath).close()
                Source.fromFile("./syntax1.txt").close()
                Source.fromFile("./semantic1.txt").close()
                if (fileContents == semanticContents) {
                    println("#semantic_error#")
                    sys.exit(200)
                }
                if (fileContents == syntaxContents) {
                    println("#syntax_error#")
                    sys.exit(100)
                }
                
                parser.parse(fileContents) match {
                case Success(x) => 
                    println("file content is")
                    println(s"$fileContents = $x")
                case Failure(msg) => println(msg)
                }
            case None => println("please enter a file name")
        }
    }
}
