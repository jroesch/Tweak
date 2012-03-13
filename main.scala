import unfiltered.request._
import unfiltered.response._
import tweak.compiler.TParser 
import scala.io.Source._

object Main {
  def main(args: Array[String]) {
    unfiltered.jetty.Http.local(8080).filter(REPLServer).run()
  }
}

object REPLServer extends unfiltered.filter.Plan {
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
}


/* val contents = 
  if (args.length > 1) 
    fromFile(args(1)).mkString 
  else 
    fromFile("test.twk").mkString
println(TParser.parseAll(TParser.all, contents)) */