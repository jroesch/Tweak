package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._

trait ExpParser extends TweakParser {
  /* XXX: to self type or to not self type? */
  this: FunctionParser with MatchParser with SymbolParser with LiteralParser => 

  val exp: GLLParser[Exp] 
}

trait DefaultExpParser extends ExpParser {
  this: FunctionParser with MatchParser with SymbolParser with LiteralParser => 

  lazy val exp: GLLParser[Exp] = (
      symbol
    | num
    | function
    | exp ~ exp ^^ { (f, g) => ApplyStream.genStream(f, g) }
    | "match" ~ exp ~ "with" ~ matcher ^^ { (_, sc, _, ms) => CaseExp(sc, ms) }
    //| "let" ~ decs ~ "in" ~ exp ~ "end" ^^ null
    | "(" ~ ")" ^^ { (_, _) => UnitL }
    | "(" ~ exp ~ ")" ^^ { (_, e, _) => ApplyStream(Vector(e)) }
    | "(" ~> commaExps <~ ")" ^^ { TTuple(_) }
  ) 

  lazy val commaExps: GLLParser[Seq[Exp]] = 
    ((exp ~ "," ~ exp)) ~ (extExp*) ^^ { (e1, _, e2, es) => 
      e1 +: e2 +: es
    }

  lazy val extExp: GLLParser[Exp] = 
    "," ~ exp ^^ { (_, e) => e }
}

