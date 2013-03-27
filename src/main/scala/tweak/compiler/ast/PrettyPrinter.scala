package tweak.compiler.ast

object PrettyPrinterP {
  import tweak.compiler.frontend.Parser.precTable
  /* print this */
  /* trait Printer[A] {
    def code(t: Tree): String 
  }
  
  implicit object ProgramPrinter extends Printer[Program] {

  }

  implicit object BindingPrinter extends Printer[Binding] {

  }
  
  implicit object Exp extends Printer[Binding] {

  } */
  
  /* research better pretty printers */
  def code(t: Tree): String = {
    import tweak.compiler.ast._

    var _code = ""
    var indentL = 0

    def codeStep(t: Tree): String = t match {
      case Program(bs) => bs.foldLeft(_code) { (c: String, b: Binding) =>
        c + codeStep(b)
      }

      case Binding(pat, _, e) => {
        newline("val " + codeStep(pat) + " = " + codeStep(e))
      }

      case p: Pattern => p match {
        case LiteralPattern(l) => codeStep(l)
        case PatternSeq(ps) => "<pattern_seq>"
        case IdPattern(ident) => codeStep(ident)
        case WildCardPattern => "_"
        case ConsPattern(h, tail) => codeStep(h) + " :: " + codeStep(tail)
        case _ => "<pattern>"
      }

      case n: Number => n.toString

      case sym: Symbol => sym.name
      
      case TFunction(ms) => "" + indent {
          ms.foldLeft("") { (a, e) => a + codeStep(e) }
      }

      case MatchExp(e, ms) => newline("match " + codeStep(e) + " with") +
        indent { 
          // XXX: Fix this so they are printed out properly"
          ms.foldLeft("") { (a, e) => a + codeStep(e) }
        }
        
      case Match(pat, e) => newline(codeStep(pat) + " => " + codeStep(e))
      
      case TTuple(es) => "(" + es.map(codeStep(_)).mkString(", ") + ")"

      case UnitL => "()"

      case other => other.toString
    }

    def indent(f: => String): String = {
      indentL += 1

      val code = f.lines.map { s => 
        val c = "  " * indentL + s
        //println("|  " + c)
        c 
      }

      indentL -= 1
      code.mkString("\n")
    }

    def newline(s: String) = s + "\n"

    "=> " + codeStep(t)
  }
  
}
