package wacc

import parsley.{Success, Failure}
import scala.io.Source
object Main {
    def main(args: Array[String]): Unit = {
        println("hello WACC!")

        args.headOption match {
            case Some(filePath) =>
                val fileContents: String = Source.fromFile(filePath).mkString
                Source.fromFile(filePath).close()

                val result = parser.parse(fileContents)

                result match {
                    case Success(x) => 
                        println("file content is")
                        println(s"$fileContents = $x")
                    case Failure(msg) => 
                        println(msg)
                        println ("# + result + #")
                        sys.exit(100)
                        // println(msg)
                    }
            case None => println("please enter a file name")
        }
    }
}
