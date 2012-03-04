import tweak.compiler.TParser 
import scala.io.Source._

object TestEnv {
  def main(args: Array[String]) {
     val contents = 
       if (args.length > 1) 
         fromFile(args(1)).mkString 
       else 
         fromFile("test.twk").mkString
     println(TParser.parseAll(TParser.all, contents))
  }
}