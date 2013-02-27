package tweak.compiler.frontend.internal

import tweak.compiler.ast.{ Infix, Symbol }

class PrecedenceTable(ps: Infix*) {
  import scala.collection.mutable.{ HashMap => Map }
  
  type PrecLevel = Int
  
  /* Precedence Levels */
  val maxLevel = 10
  
  val defaultLevel = 4
  
  val minLevel = 0
  
  var _internalMap = Map[Symbol, Int] {
    (for (Infix(sym, pl) <- ps) yield sym -> pl): _*
  }
  
  def apply(sym: Symbol): PrecLevel = 
    _internalMap.get(sym).getOrElse(defaultLevel)
  
  def update(sym: Symbol, pl: PrecLevel) =
    _internalMap(sym) = pl
  
  def atLevel(level: PrecLevel) = 
    for ((symb, lvl) <- _internalMap if lvl == level) yield symb
}