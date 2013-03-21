package tweak.compiler.backend

import tweak.compiler.ast._
import com.codecommit.gll._

trait Emitter {
  /* type Emitted[A]

  def emitLiteral[A <: Literal](l: A): Emitted[A]

  def emitFunction(f: TFunction): Emitted[ */
}

trait ScalaEmitter extends Emitter {
  
  def unpack[A](s: Stream[Result[A]]) = s match {
    case Success(r, _) #:: _ => r
  }

  def emitProgram(p: Program): String = p match {
    case Program(bs) => bs.foldLeft("") { (code: String, b: Binding) =>
      code + emitBinding(b)
    }
  }

  def emitBinding(b: Binding): String = b match {
    case Binding(pat, _, exp) => "val " + emitPattern(pat) + " = " + emitExpression(exp)
  }

  def emitPattern(p: Pattern) = p match {
    case LiteralPattern(lit)     => emitLiteral(lit)
    case PatternSeq(ps)          => emitPatternSeq(ps)
    case IdPattern(id)           => emitIdentifier(id)
    case WildCardPattern         => emitWildCardPattern
    /* XXX: this should be refactored out from built-in syntax to a stdlib cons */
    case ConsPattern(hpat, tpat) => emitConsPattern(hpat, tpat)
  }
  
  private def emitPatternSeq(ps: Seq[Pattern]) = ???

  private def emitWildCardPattern = "_"

  private def emitConsPattern(x: Pattern, xs: Pattern) = ???

  def emitExpression(e: Exp): String = e match {
    case a: Apply   => emitApply(a)
    case l: Literal => emitLiteral(l)
  }
  
  /* this needs to not be recursive? */
  def emitApply(ap: Apply) = ap match {
    case Apply(f, g) => emitExpression(f) + emitExpression(g)
  }

  def emitIdentifier(id: Id) = id.str

  def emitOperator(op: Op) = ???

  def emitLiteral(lit: Literal) = lit match {
    case s: StringL => emitStringL(s)
    case n: Number  => emitNumber(n)
    case UnitL      => emitUnitL
  }

  /* XXX: smarter then toString in general */
  private def emitNumber(num: Number) = num.toString

  /* easy impls for Scala */
  private def emitIntL(i: IntL) = i.toString

  private def emitDoubleL(d: DoubleL) = d.toString

  private def emitStringL(s: StringL) = s.s

  private def emitUnitL = "()"

    /* need to properly resolve names at some point */
    /* XXX: this is a sprint to MVP functionality, 
     * I want this to be testable and working top to bottom
     * before too much design is done, easier to develop a 
     * live workin implementation, many re-writes will come */

}

object JavaScriptEmitter extends Emitter {

}
