package tweak.internal.types

trait IType[+T] 

case class IDouble(val d: Double) extends IType[Double]

case class IInt(val i: Int) extends IType[Int]

case class IString(val s: String) extends IType[String]

case class ISymbol(val s: Symbol) extends IType[Symbol]

case class IBoolean(val b: Boolean) extends IType[Boolean]

