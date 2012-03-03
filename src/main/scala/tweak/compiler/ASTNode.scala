package tweak.compiler

trait ASTNode

class ValueNode[T <: TweakValue](val valueType: Symbol, value: T)