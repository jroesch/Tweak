import tweak.compiler.TweakParser 
import scala.io.Source._

object TestEnv {
  def main(args: Array[String]) {
     val contents = 
       if (args.length > 1) 
         fromFile(args(1)).mkString 
       else 
         fromFile("test.twk").mkString
     println(TweakParser.parseAll(TweakParser.all, contents))
  }
}