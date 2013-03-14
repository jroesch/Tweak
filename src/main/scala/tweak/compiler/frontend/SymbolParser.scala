package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._


/* find better naming */
trait Name extends TweakParser {
  lazy val name: GLLParser[Var] = (
      id
    | op
  )
  
  val operator: GLLParser[String] = """[=\+\*\-\?\\\$<>:]+"""r
  
  val identifier: GLLParser[String] = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r

}
