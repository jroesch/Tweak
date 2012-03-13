package tweak.compiler.ast

import tweak.util.{ StringUtil => Util }

class ListNode[T](s: Seq[T]) extends BaseNode {
  val internalList = for(each <- s) yield each //.eval
  //def eval: Option[IType] = None 
  override def toString = "[" + Util.join(s, ",") + "]"
} 