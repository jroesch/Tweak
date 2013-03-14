package tweak.compiler.frontend

import com.codecommit.gll._

trait TweakParser extends RegexParsers {
  type GLLParser[T] = Parser[T]
}
