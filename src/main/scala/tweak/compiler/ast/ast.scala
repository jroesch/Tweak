package tweak.compiler

import language.implicitConversions

package object ast {
  
  trait PrettyPrintable {
    def pprint: String = this.toString
  }
  
  sealed trait Term extends PrettyPrintable
  
  /* Program */
  case class Program(es: Seq[Binding]) extends Term
  
  /* Expressions */
  sealed trait Exp extends Term
  case class Op(op: Symbol) extends Exp
  case class CaseExp(scrutinee: Any, matches: Any) extends Exp
  case class AssignExp(name: Any, value: Any) extends Exp
  
  /* Functions */
  case class TFunction(ms: Seq[Match]) extends Exp
  
  case class Apply(a: Exp, b: Exp) extends Exp {
    override def pprint = a match {
      case Apply(Apply(op: Op, e1), e2) => 
        e1.pprint + " " + op.pprint + " " + e2.pprint
      case Apply(f, g) => f.pprint + " " + g.pprint
    }
    
    // Shift reduce parser for operators?
    /* case (level, Apply(Apply(op, e1) e2)) => self(level, e2)
    case (level, e) => */
    
    def applyTo(e: Exp): Apply = Apply(this, e)
  }
  
  
  /* Match */
  case class Match(pat: Pattern, exp: Exp) extends Term
  case class MatchSeq(ps: Seq[Pattern]) extends Term
  
  /* Pattern */
  sealed trait Pattern extends Term
  case class LiteralPattern(l: Literal) extends Pattern
  case class PatternSeq(pat: Seq[Term]) extends Pattern
  case class IdPattern(id: Id) extends Pattern
  case object WildCardPattern extends Pattern
  case class ConsPattern(head: Pattern, tail: Pattern) extends Pattern
  
  /* Identifier */
  case class Symbol(s: String) extends Term
  case class Id(s: Symbol) extends Exp
  
  /* Variable */
  case class Binding(pat: Pattern, typ: Type = AnyType, exp: Exp) extends Term
  
  /* Type */
  sealed trait Type extends Term
  case object AnyType extends Type
  
  /* Literals */
  sealed trait Literal extends Exp
  sealed trait Number extends Literal
  case class IntL(i: Int) extends Number
  case class DoubleL(d: Double) extends Number
  case class StringL(s: String) extends Literal
  case object UnitL extends Literal
  
  /* Literal Implicits */
  implicit def intToIntL(i: Int): IntL = IntL(i)
  implicit def doubleToDoubleL(d: Double): DoubleL = DoubleL(d)
  implicit def stringToStringL(s: String): StringL = StringL(s)
}