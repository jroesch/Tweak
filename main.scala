//import unfiltered.request._
//import unfiltered.response._
import tweak.compiler.TParser
import scala.tools.jline.console.ConsoleReader
import scala.io.Source._


object Main {
  def main(args: Array[String]) {
    //unfiltered.jetty.Http.local(8080).filter(REPLServer).run()
    val reader = new ConsoleReader();
    reader.setPrompt("> ")
    var line = reader.readLine()
    while (line != null) {
      println(TParser.parseAll(TParser.all, line))
      line = reader.readLine()
    }
  } 
}

/* object REPLServer extends unfiltered.filter.Plan {
  def intent = {
    case req @ Path("/") => req match {
      case POST(_) => {
        val Params(params) = req
        val result = repl(params("code").mkString)
        Ok ~> ResponseString(result)
      }
    }
  }
  
  def repl(s: String): String = {
    TParser.interpret(s).toString
  }
} */
