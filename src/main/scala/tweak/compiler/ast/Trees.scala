package tweak.compiler

/* architecture Borrowed from the Scalac */
trait Trees {
   sealed trait Tree 
  
  /* Program */
  case class Program(es: Seq[Binding]) extends Tree

  /* Variable */
  case class Binding(pat: Pattern, typ: Type = AnyTypE, exp: Exp) extends Tree
  
  /* Expressions */
  sealed trait Exp extends Tree

  /* Function */
  case class Function(ms: Seq[Match]) extends Exp

  /* Match */
  case class Match(pat: Pattern, exp: Exp) extends Tree 

  /* Pattern */
  sealed trait Pattern extends Tree
  case class LiteralPattern(l: Literal) extends Pattern
  case class PatternSeq(pat: Seq[Pattern]) extends Pattern
  case class IdPattern(id: Id) extends Pattern
  case object WildCardPattern extends Pattern
  case class ConsPattern(head: Pattern, tail: Pattern) extends Pattern

  /* OpParser Scaffolding */
  sealed trait InfixExp extends Exp
  case class Only(e: Exp) extends InfixExp
  case class Neg(e: InfixExp) extends InfixExp
  case class Infix(i: InfixExp, o: Op, e: Exp) extends InfixExp

  case class Negate(e: Exp) extends Exp
  case class Apply(e: Exp, f: Exp) extends Exp
  case class OpApply(o: Op, e: Exp, f: Exp) extends Exp

  case class OpError(msg: String) extends Exp
  //case class AssignExp(name: Any, value: Any) extends Exp
  
  
  // XXX: enable sections
  case class Section(o: Op) extends Exp
  
  /* Symbols */
  class Symbol(val name: String) extends Exp
  case class Id(str: String) extends Symbol(str)
  case class Op(str: String) extends Symbol(str)
  
  
  /* Type */
  sealed trait Type extends Tree
  case object AnyType extends Type
  
  sealed trait TList extends Exp
  case class TCons(head: Exp, tail: Exp) extends Value
  case object TNil extends Value with Literal

  /* Tuple Wraper */
  case class TTuple[A <: Exp](tup: Seq[A]) extends Exp {
    val size = tup.length

    if (size > 23) throw new Exception("Too big mannnn!")
  }
  /* fix literal inheritance BS */
  sealed trait Literal extends Exp

  case class Integer(i: Int) extends Literal {
    override def toString = i.toString
  }
  
  case class Float(d: Double) extends Literal {
    override def toString = d.toString
  }
  
  case class String(s: String) extends Literal {
    override def toString = s.toString
  }
  
  case object TUnit extends Literal {
    override def toString = "()"
  }
  
  /* Literal Implicits */
 // implicit def intToIntL(i: Int): IntL = IntL(i)
 // implicit def doubleToDoubleL(d: Double): DoubleL = DoubleL(d)
 // implicit def stringToStringL(s: String): StringL = StringL(s)
 // implicit def intLToInt(i: IntL): Int = i.i
 // implicit def doubleLToDouble(d: DoubleL): Double = d.d
 // implicit def strLToString(s: StringL): String = s.s

  type TreeCopier <: TreeCopierOps

  abstract class Transformer {
    val treeCopy: TreeCopier = new LazyTreeCopier

    def transform(tree: Tree): Tree = itransform(this, tree)

    def transformTrees(trees: List[Tree]): List[Tree] =
      if (tree.isEmpty) Nil else trees mapConserve transform

    protected def itransform(transformer: Transformer, tree: Tree): Tree = {
      import transformer._
      val treeCopy = transformer.treeCopy

      tree match {
        case Program
        case Binding
        case _ => throw new Exception("misisng a case") 
      }
    }
  }

}
