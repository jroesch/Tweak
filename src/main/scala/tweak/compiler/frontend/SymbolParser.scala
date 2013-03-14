package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._

trait SymbolParser extends TweakParser {
  val symbol: GLLParser[Symbol] 
  val id: GLLParser[Id]
  val op: GLLParser[Op]
}

trait DefaultSymbolParser extends SymbolParser {
  lazy val symbol = (
      id
    | op
  )

  lazy val id = identifier ^^ { i => Id(i) }

  lazy val op = operator ^^ { o => Op(o) }

  val operator: GLLParser[String] = """[=\+\*\-\?\\\$<>:]+"""r
  
  val identifier: GLLParser[String] = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
}
  
