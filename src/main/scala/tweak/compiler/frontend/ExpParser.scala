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
      function
    | exp ~ exp ^^ null
    | exp ~ op ~ exp ^^ { (e1, op, e2) => 
        e1 match {
          case ie: InfixExp => Infix(ie, op, e2)
          case _            => Infix(Only(e1), op, e2) 
        }
      }
    | "match" ~ exp ~ "with" ~ matcher ^^ { (_, sc, _, ms) => MatchExp(sc, ms) }
    | "(" ~ ")" ^^ { (_, _) => UnitL }
    | section
    | "(" ~ exp ~ ")" ^^ null
    | "(" ~> commaExps <~ ")" ^^ { TTuple(_) }
    | id
    | num
  ) 

  lazy val section: GLLParser[Section] =  "(" ~> op <~ ")" ^^ { o => Section(o) }

  lazy val commaExps: GLLParser[Seq[Exp]] = 
    ((exp ~ "," ~ exp)) ~ (extExp*) ^^ { (e1, _, e2, es) => 
      e1 +: e2 +: es
    }

  lazy val extExp: GLLParser[Exp] = 
    "," ~ exp ^^ { (_, e) => e }
}

