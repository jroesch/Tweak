package tweak.compiler.ast

object PrettyPrinterP {
  /* print this */
  /* trait Printer[A] {
    def code(t: Term): String 
  }
  
  implicit object ProgramPrinter extends Printer[Program] {

  }

  implicit object BindingPrinter extends Printer[Binding] {

  }
  
  implicit object Exp extends Printer[Binding] {

  } */
  
  /* research better pretty printers */
  def code(t: Term): String = {
    import tweak.compiler.ast._

    var _code = ""
    var indentL = 0

    def codeStep(t: Term): String = t match {
      case Program(bs) => bs.foldLeft(_code) { (c: String, b: Binding) =>
        c + codeStep(b)
      }

      case Binding(pat, _, e) => {
        newline("val " + codeStep(pat) + " = " + codeStep(e))
      }

      case p: Pattern => "<pattern>"

      case n: Number => n.toString
      
      case TFunction(ms) => indent {
        ms.foldLeft(_code) { (c, m) =>
          c + codeStep(m)
        }
      }
      
      case Match(pat, e) => codeStep(pat) + " => " + codeStep(exp)
      case TTuple(es) => "(" + es.map(codeStep(_)).mkString(", ") + ")"

      case UnitL => "()"

      case other => other.toString
    }
    def indent(f: () => String) {
      indentL += 1
      val code = f().lines.map { s => ("\t" * indentL) + s }
      indentL -= 1
      res
    }

    def newline(s: String) = s + "\n"

    codeStep(t)
  }
  
}
