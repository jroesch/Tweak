package tweak.compiler

package object ast {
  sealed trait Node
  
  /* Expressions */
  sealed trait Exp extends Node
  case class BinOp extends Exp
  case class CaseExp(scrutinee: Any, binders: Any) extends Exp
  case class AssignExp(name: Any, value: Any) extends Exp
  
  /* Functions */
  case class Function(params: Seq[Any], body: Seq[Exp]) extends Node
  case class FunctionCall(callee: Function, args: Seq[Exp])
  
  /* Pattern */
  sealed trait Pattern extends Node
  case class LiteralPattern(l: Literal) extends Pattern
  case class PatternSeq(pat: Seq[Node]) extends Pattern
  case class IdPattern(id: Id) extends Pattern
  case object WildCard extends Pattern
  
  /* Identifier */
  case class Symbol(s: String) extends Node
  case class Id(s: Symbol) extends Node
  
  /* Variable */
  case class Binding(pat: Pattern, typ: Type = AnyType, exp: Exp)
  
  /* Type */
  sealed trait Type extends Node
  case object AnyType extends Type
  
  /* Literals */
  sealed trait Literal extends Node
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