package tweak.compiler.ast
import tweak.internal.types._
//look at view bounds

class ValueNode(val value: IType) extends BaseNode {
  //def eval: IType = value
  override def toString = value.toString
} 

//class DoubleNode extends ValueNode[Double]