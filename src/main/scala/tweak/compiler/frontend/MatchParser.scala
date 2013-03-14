package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._

trait MatchParser { this: ExpParser with SymbolParser with LiteralParser =>
  val matcher: GLLParser[Seq[Match]]
  val mrule: GLLParser[Match]
  val pattern: GLLParser[Pattern]
}

trait DefaultMatchParser extends MatchParser { this: ExpParser with SymbolParser with LiteralParser =>
  lazy val matcher: GLLParser[Seq[Match]] = (
      mrule ^^ { m => Vector(m) }
    | mrule ~ "|" ~ matcher ^^ { (m, _, ms) => m +: ms }
  )
  
  lazy val mrule = pattern ~ "=>" ~ exp ^^ { (pat, _, exp) => Match(pat, exp) }
          
  /* pattern is either a literal or wildcard */
  lazy val pattern: GLLParser[Pattern] = (
      "_" ^^ { _ => WildCardPattern }
    | id  ^^ { ident => IdPattern(ident) }
    | num ^^ { n => LiteralPattern(n) }
    | "(" ~ ")" ^^ { (_, _) => LiteralPattern(UnitL) }
    | "(" ~ commaPats ~ ")" ^^ { (_, ps, _) => PatternSeq(ps) }
    //| "nil"
    | pattern ~ "::" ~ pattern ^^ { (head, _, tail) => ConsPattern(head, tail) }
  )
  
  lazy val commaPats: GLLParser[Seq[Pattern]] = (
      pattern ^^ { p => Vector(p) }
    | commaPats ~ "," ~ pattern ^^ { (ps, _, p) => ps :+ p }
  )
}
