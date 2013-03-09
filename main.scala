import tweak.compiler.frontend.{ Parser => TweakParser }
import com.codecommit.gll._
import tweak.compiler.ast._
import scala.tools.jline.console.ConsoleReader
import scala.io.Source._
import tweak.compiler.ast.PrettyPrinterP.{ code => pcode }

object Main {
  def main(args: Array[String]) {
    //unfiltered.jetty.Http.local(8080).filter(REPLServer).run()
    if (args.length > 0) {
      val results = TweakParser.program(fromFile(args(0)).mkString)
      val forest = for (Success(tree, _) <- results) yield tree
      forest.head match {
        case p@Program(xs) => println(pcode(p)) //xs.foreach(println(_))
        case _ => println("failed")
      }
      
    } else {
      val reader = new ConsoleReader();
      reader.setPrompt("> ")
      var line = reader.readLine()
      while (line != null) {
        val results = TweakParser.interpreter(line)
        results map { case Success(value, _) => println(value); case _ => Unit }
        line = reader.readLine()
      }
    }
  } 
}
