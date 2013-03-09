package tweak.compiler

import language.implicitConversions

package object ast {
  
  trait PrettyPrintable {
    def pprint: String = this.toString
  }
  
  object PrettyPrinter {
    import com.codecommit.gll._
    def prettyPrint[A <: PrettyPrintable](s: Stream[Result[A]]) = s match {
      case Stream(s) => s match { 
        case Success(v, rest) => v.pprint
        case _                => throw new Exception("parse failed at:")
      }
      case _ => throw new Exception("match error")
    }
  }
  
  sealed trait Term extends PrettyPrintable { this: Term => 
    override def pprint: String = {
      
      var ilevel = 0
      
      def ipprint(t: Term, indentLevel: Int = ilevel): String = t match {
          case Program(es) => es.foldLeft("") { (a, s) => a + ipprint(s) + "\n" } 
            // case
          case _ => (t, indentLevel).toString
      }

      ipprint(this)
    }
  }
  
  /* Program */
  case class Program(es: Seq[Binding]) extends Term
  
  /* Expressions */
  sealed trait Exp extends Term
  case class Op(op: Symbol) extends Exp
  case class CaseExp(scrutinee: Any, matches: Any) extends Exp
  case class AssignExp(name: Any, value: Any) extends Exp
  
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
  }
  
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
  
  case class Infix(s: Symbol, prec: Int)
  
  /* Variable */
  case class Binding(pat: Pattern, typ: Type = AnyType, exp: Exp) extends Term
  
  /* Type */
  sealed trait Type extends Term
  case object AnyType extends Type
  
  /* Literals */
  sealed trait Value extends Exp
  /* Tuple Wraper */
  case class TTuple[A <: Exp](tup: Seq[A]) extends Exp {
    val size = tup.length

    if (size > 23) throw new Exception("Too big mannnn!")
  }

  sealed trait Literal extends Value
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
