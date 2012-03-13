package tweak.internal.types

trait IType

case class IDouble(val d: Double) extends IType

case class IInt(val i: Int) extends IType

case class IString(val s: String) extends IType

case class ISymbol(val s: Symbol) extends IType

case class IBoolean(val b: Boolean) extends IType

