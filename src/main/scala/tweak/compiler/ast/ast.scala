package tweak.compiler

import language.implicitConversions

/* I hate this file, but everything must be in the same file in order to be sealed */
package object ast {
  
  sealed trait Term 
  
  /* Program */
  case class Program(es: Seq[Binding]) extends Term
  
  /* Expressions */
  sealed trait Exp extends Term
  
  /* OpParser Scaffolding */
  sealed trait InfixExp extends Exp
  case class Only(e: Exp) extends InfixExp
  case class Neg(e: InfixExp) extends InfixExp
  case class Infix(i: InfixExp, o: Op, e: Exp) extends InfixExp

  case class Negate(e: Exp) extends Exp
  case class Apply(e: Exp, f: Exp) extends Exp
  case class OpApply(o: Op, e: Exp, f: Exp) extends Exp {
    def asSource: String = {
      def step(e: Exp): String = e match {
        case OpApply(o, e1, e2) => "(" + step(e1) + " " + o.name + " " + step(e2) + ")"
        case e => e.toString
      }

      step(this)
    }
  }

  case class OpError(msg: String) extends Exp
  
  case class MatchExp(scrutinee: Exp, matches: Seq[Match]) extends Exp 
  //case class AssignExp(name: Any, value: Any) extends Exp
  
  /* Functions */
  case class TFunction(ms: Seq[Match]) extends Exp
  
  // XX: enable sections
  case class Section(o: Op) extends Exp
  
  /* Match */
  case class Match(pat: Pattern, exp: Exp) extends Term
  
  /* Pattern */
  sealed trait Pattern extends Term
  case class LiteralPattern(l: Literal) extends Pattern
  case class PatternSeq(pat: Seq[Term]) extends Pattern
  case class IdPattern(id: Id) extends Pattern
  case object WildCardPattern extends Pattern
  case class ConsPattern(head: Pattern, tail: Pattern) extends Pattern
  
  /* Symbols */
  class Symbol(val name: String) extends Exp {
    /* def unapply(...) = ???
     */ }
  case class Id(str: String) extends Symbol(str)
  case class Op(str: String) extends Symbol(str)
  
  /* Variable */
  case class Binding(pat: Pattern, typ: Type = AnyType, exp: Exp) extends Term
  
  /* Type */
  sealed trait Type extends Term
  case object AnyType extends Type
  
  /* Literals */
  sealed trait Value extends Exp

  sealed trait TList extends Value
  case class TCons(head: Exp, tail: Exp) extends Value
  //case class TListL extends Value with Literal
  case object TNil extends Value with Literal

  /* Tuple Wraper */
  case class TTuple[A <: Exp](tup: Seq[A]) extends Exp {
    val size = tup.length

    if (size > 23) throw new Exception("Too big mannnn!")
  }
  /* fix literal inheritance BS */
  sealed trait Literal extends Exp

  sealed trait Number extends Literal

  case class IntL(i: Int) extends Number {
    override def toString = i.toString
  }
  
  case class DoubleL(d: Double) extends Number {
    override def toString = d.toString
  }
  
  case class StringL(s: String) extends Literal {
    override def toString = s.toString
  }
  
  case object UnitL extends Literal {
    override def toString = "()"
  }
  
  /* Literal Implicits */
  implicit def intToIntL(i: Int): IntL = IntL(i)
  implicit def doubleToDoubleL(d: Double): DoubleL = DoubleL(d)
  implicit def stringToStringL(s: String): StringL = StringL(s)
  implicit def intLToInt(i: IntL): Int = i.i
  implicit def doubleLToDouble(d: DoubleL): Double = d.d
  implicit def strLToString(s: StringL): String = s.s
}
