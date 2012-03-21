package tweak.internal.types

sealed trait IRef extends IType
case class IVar(val id: IIdentifier, val tType: IType) extends IRef
case class IVal(val id: IIdentifier, val tType: IType) extends IRef 