package tweak.compiler.frontend

import com.codecommit.gll._
import tweak.compiler.ast._
import language.postfixOps

object Parser extends DefaultExpParser
    with DefaultFunctionParser
    with DefaultMatchParser
    with DefaultSymbolParser
    with DefaultLiteralParser {
  /* implicit class UnwrapResult[A <: Term](val r: Result[A]) extends AnyVal {
    def result: Term = r match {
      case Success
    }
  } */

  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r
  
  lazy val interpreter = exp | dec
  
  lazy val program: GLLParser[Program] = (dec*) ^^ { ds => Program(ds) }
  
  lazy val decs = dec*
  
  lazy val dec: GLLParser[Binding] = (
      "val" ~ pattern ~ "=" ~ exp ^^ { (_, p, _, e) => Binding(pat = p, exp = e) }
    | "val" ~ id ~ "=" ~ function ^^ { 
      (_, ident, _, f) => Binding(pat = IdPattern(ident), exp = f) 
    }
  )
  
  //def mkErrorMsg(failed: Failure) = {
  //  val Failure(data, tail) = failed
  //  val expected = data match {
  //    case ExpectedRegex(regex) => s"Expected the_regex on "
  //  }
  //}
}
