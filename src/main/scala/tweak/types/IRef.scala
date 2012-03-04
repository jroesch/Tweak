package tweak.internal.types

sealed trait IRef
case class IVar(IIdentifier, IType) extends IRef
case class IVal(IIdentifier, IType) extends IRef 