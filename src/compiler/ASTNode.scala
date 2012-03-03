import tweak.lang.compiler._

trait ASTNode

class ValueNode[T](val type: Symbol, val value: TweakValue) extends ASTNode