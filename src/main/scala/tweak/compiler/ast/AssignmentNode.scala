package tweak.compiler.ast

/* import tweak.internal.types._

abstract class AssignmentNode(id: IIdentifier, value: IType) extends BaseNode {
  val ref: IRef 
  val identifier: IIdentifier = id
  val value: IType = value
  //function that adds name to enclosing env
  def define
}

class ValNode(id: IIdentifier, value: IType) extends AssignmentNode(id, value) {
  val ref = new IVal(id, value)
  
  def eval: Option[IType] = {
    //add to global env
  }
}

class VarNode(id: IIdentifier, value: IType) extends AssignmentNode(id, value) {
  val ref = new IVar(id, value)
  
  def eval: Option[IType] = {
    //add to global env, maybe factor both in AssingmentNode
  }
} */