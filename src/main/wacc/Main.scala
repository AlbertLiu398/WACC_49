package wacc

import parsley.{Success, Failure}
import scala.io.Source
import parser._
object Main {
    def main(args: Array[String]): Unit = {
        println("hello WACC!")
        
        // println (paramParse("int x"))
        // println (parse("begin pair(int , int) p = newpair(10, 3) end"))
        // println (allTypeParse("pair(int,int)"))
        println (funcParse("int f(int x) is skip end"))
  

        
        val filePath = "file.txt"
        val fileContents: String = Source.fromFile(filePath).mkString
        println(fileContents)
        Source.fromFile(filePath).close()

        args.headOption match {
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => println(s"$expr = $x")
                case Failure(msg) => println(msg)
            }
            case None => println("please enter an expression")
        }
    }
}
