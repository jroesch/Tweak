package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._

trait FunctionParser extends TweakParser { this: MatchParser =>
  val function: GLLParser[TFunction]
}

trait DefaultFunctionParser extends FunctionParser { this: MatchParser =>
  lazy val function = 
    matcher ^^ { ms => TFunction(ms) }
}
