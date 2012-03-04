package tweak.compiler.ast

class ValueNode[T](val value: T) extends BaseNode {
  override def toString = value.toString
}