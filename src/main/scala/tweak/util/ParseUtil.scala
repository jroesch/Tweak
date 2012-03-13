package tweak.util

import tweak.util.StringUtil.stripQuotes
import tweak.compiler.ast._
import tweak.internal.types._

object ParseUtil {
  def newIntegerNode(s: String) = new ValueNode(IInt(s.toInt))
  def newDoubleNode(s: String) = new ValueNode(IDouble(s.toDouble))
  def newStringNode(s: String) = new ValueNode(IString(stripQuotes(s)))
  def newSymbolNode(s: String) = new ValueNode(ISymbol(Symbol(stripQuotes(s))))
  def newBooleanNode(b: Boolean) = new ValueNode(IBoolean(b))
}