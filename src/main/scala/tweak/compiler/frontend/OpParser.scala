package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._
import tweak.compiler.frontend.internal._
import language.implicitConversions

trait OpParser {
  type Precedence = Int
  /* XXX: Not thread safe in the slightest, icky mutable state */
  val precTable = new PrecedenceTable
  /* for current testing purposes */
 /* fix implicits issues */
  precTable("+") = 6 -> L 
  precTable("-") = 6 -> L
  precTable("*") = 2 -> L

  def tidy(ie: InfixExp, oes: List[(Op, Exp)]): Exp = (ie, oes) match {
    case (Only(a), Nil)                 => a
    case (Only(a), (o, b) :: ss)        => tidy(Only(OpApply(o, a, b)), ss)
    case (Infix(a, o, b), Nil)          => tidy(a, List((o, b)))   
    case (Infix(a, o, b), (p, c) :: ss) => true match {
      case _ if shift(o, p)  => tidy(a, (o, b) :: (p,c) :: ss)
      case _ if reduce(o, p) => tidy(Infix(a, o, OpApply(p, b, c)), ss)
      case _ if ambig(o,p)   => OpError("ambig use of ops")
    }
    case (Neg(e), Nil)                  => tidy(tidyNeg(e), Nil)
    case (Neg(e), (o,b) :: ss)          => true match {
      case _ if nshift(o) => tidy(Neg(underNeg(o, b, e)), ss)
      case _ if nreduce(o)   => tidy(tidyNeg(e), (o, b) :: ss)
      case _ if nambig(o) => OpError("illegal use of negation")
    }
  }

  def prec(o: Op): Precedence = precTable(o.name)._1

  def assoc(o: Op): Assoc = precTable(o.name)._2

  def shift(o: Op, p: Op) = (prec(o) > prec(p)) || 
    (prec(o) == prec(p) && assoc(o) == L && assoc(p) == L)

  def reduce(o: Op, p: Op) = (prec(o) < prec(p)) || 
    (prec(o) == prec(p) && assoc(o) == R && assoc(p) == R)

  def ambig(o: Op, p: Op) = (prec(o) == prec(p)) &&
    (assoc(o) == N || assoc(p) == N || assoc(o) != assoc(p))

  def nshift(o: Op) = prec(o) > 6

  def nreduce(o: Op) = (prec(o) < 6) || (prec(o) == 6 && assoc(o) == L)

  def nambig(o: Op) = prec(o) == 6 && (assoc(o) == R || assoc(o) == N)

  def tidyNeg(o: InfixExp): InfixExp = o match {
    case Only(e)        => Only(Negate(e))
    case Infix(a, o, b) => Infix(a, o, Negate(b))
    case Neg(e)         => tidyNeg(tidyNeg(e))
  }

  def underNeg(o: Op, b: Exp, oe: InfixExp): InfixExp = oe match {
    case Only(e)        => Only(OpApply(o, e, b))
    case Neg(e)         => Neg(underNeg(o, b, e))
    case Infix(e, p, f) => Infix(e, p, OpApply(o, f, b))
  }
}

