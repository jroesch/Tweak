import tweak.compiler.frontend.{ TParser => TweakParser }
import scala.tools.jline.console.ConsoleReader
import scala.io.Source._

object Main {
  def main(args: Array[String]) {
    //unfiltered.jetty.Http.local(8080).filter(REPLServer).run()
    if (args.length > 0) {
      println(TweakParser.program(fromFile(args(0)).mkString))
    } else {
      val reader = new ConsoleReader();
      reader.setPrompt("> ")
      var line = reader.readLine()
      while (line != null) {
        println(TweakParser.dec(line).head)
        line = reader.readLine()
      }
    }
  } 
}
