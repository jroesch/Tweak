package tweak.compiler.ast

import tweak.internal.types._

trait AssignmentNode extends BaseNode {
  val ref: IRef 
}

class ValNode(id: IIdentifier, value: IType) extends AssignmentNode {
  val ref = new IVal(id, value)
}

class VarNode(id: IIdentifier, value: IType) extends AssignmentNode {
  val ref = new IVar(id, value)
}