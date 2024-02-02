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
