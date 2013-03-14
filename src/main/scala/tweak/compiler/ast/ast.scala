package tweak.compiler

import language.implicitConversions

package object ast {
  
  sealed trait Term 
  
  /* Program */
  case class Program(es: Seq[Binding]) extends Term
  
  /* Expressions */
  sealed trait Exp extends Term
  case class CaseExp(scrutinee: Any, matches: Any) extends Exp 
  //case class AssignExp(name: Any, value: Any) extends Exp
  
  /* Functions */
  case class TFunction(ms: Seq[Match]) extends Exp
  
  object ApplyStream {
    implicit def vectorToApplyStream(vec: Vector[Exp]) = ApplyStream(vec)
    implicit def ApplyStreamToVector(opstr: ApplyStream): Vector[Exp] = opstr.ops
    
    def genStream(f: Exp, g: Exp): ApplyStream = 
      (f, g) match {
        case (ApplyStream(fst), ApplyStream(snd)) => fst ++ snd
        case (ApplyStream(fst), snd)              => fst :+ snd
        case (fst, ApplyStream(snd))              => fst +: snd
        case (_, _)                               => Vector(f, g)
      }
  }
  
  case class ApplyStream(ops: Vector[Exp]) extends Exp {
    override def toString = "ApplyStream(" + ops.mkString(", ") + ")"

     /* def fix(pt: frontend.internal.PrecedenceTable) = this match {
      (pt.minLevel to pt.maxLevel) map { l => 
        val atLevel = pt atLevel l
        
        type StreamState = (Exp, Exp, Vector[Exp])

        var ttree = Vector[Exp]()

        var i = 0;
        while (i < ops.length) {
          (ops(i), ops(i + 1), ops(i + 2)) match {
            case (e1, o: Op, e2) if pt(o.op) == l => {
              i += 3
              tree = Apply(Apply(o, e1), e2) +: tree
            }

            case e => {
              i += 1
              tree = e +: tree
            }
          }
        }

      } */

  }
  
  case class Apply(a: Exp, b: Exp) extends Exp {
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
  
  /* Symbols */
  class Symbol(s: String) extends Exp {
    /* def unapply(...) = ???
     */
  }
  case class Id(s: String) extends Symbol(s)
  case class Op(s: String) extends Symbol(s)
  
  case class Infix(s: Symbol, prec: Int)
  
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
