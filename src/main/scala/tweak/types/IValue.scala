package tweak.internal.types

trait IValue extends IType

case class IDouble(val d: Double) extends IValue

case class IInt(val i: Int) extends IValue

case class IString(val s: String) extends IValue

case class ISymbol(val s: Symbol) extends IValue

case class IBoolean(val b: Boolean) extends IValue