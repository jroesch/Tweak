package tweak.compiler.ast

import tweak.util.{ StringUtil => Util }

class ListNode[T](val s: Seq[T]) extends BaseNode {
  override def toString = "[" + Util.join(s, ",") + "]"
}