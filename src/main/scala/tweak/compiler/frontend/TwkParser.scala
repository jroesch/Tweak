package tweak.compiler.frontend

import com.codecommit.gll._
import Parsers._
import RegexParsers._
import tweak.compiler.ast._
import tweak.internal.types._
import tweak.util.StringUtil._
import language.postfixOps

object TwkParser extends RegexParsers {
  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r
  
  lazy val interpreter = exp | dec
  
  lazy val program = (dec*) ^^ { ds => Program(ds) }
  
  lazy val decs = dec*
  
  lazy val dec = (
      "val" ~ pat ~ "=" ~ exp ^^ { (_, p, _, e) => Binding(pat = p, exp = e) }
    | "val" ~ id ~ "=" ~ fun ^^ { 
      (_, ident, _, f) => Binding(pat = IdPattern(ident), exp = f) 
    }
  )
  
  lazy val fun: Parser[TFunction] = 
    matcher ^^ { ms => TFunction(ms) }
    
  lazy val exp: Parser[Exp] = (
      id
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ null
    | num
    | exp ~ op ~ exp ^^ { (e1, opr, e2) => Apply(opr, e1) applyTo e2  }
    | fun
    | exp ~ exp ^^ { (f, g) => Apply(f, g) }
    | "nil" ^^ null
    | exp ~ "::" ~ exp ^^ null
    | "case" ~ exp ~ "of" ~ matcher ^^ null
    | "let" ~ decs ~ "in" ~ exp ~ "end" ^^ null
    | "(" ~ ")" ^^ { (_, _) => UnitL }
    | "(" ~ op ~ ")" ^^ { (_, operator, _) => operator }
    | "(" ~ exp ~ ")" ^^ { (_, e, _) => e }
    | "(" ~ commaExps ~ ")" ^^ null
  )
  
  lazy val commaExps: Parser[Any] = 
    (exp ~ "," ~ exp) ~ (("," ~ exp)*) ^^ null
  
  lazy val matcher: Parser[Seq[Match]] = (
      mrule ^^ { m => Vector(m) }
    | mrule ~ "|" ~ matcher ^^ { (m, _, ms) => m +: ms }
  )
  
  lazy val mrule = pat ~ "=>" ~ exp ^^ { (pat, _, exp) => Match(pat, exp) }
          
  lazy val pat: Parser[Pattern] = (
      "_" ^^ { _ => WildCardPattern }
    | id  ^^ { ident => IdPattern(ident) }
    | num ^^ { n => LiteralPattern(n) }
    | "(" ~ ")" ^^ { (_, _) => LiteralPattern(UnitL) }
    | "(" ~ commaPats ~ ")" ^^ { (_, ps, _) => PatternSeq(ps) }
    //| "nil"
    | pat ~ "::" ~ pat ^^ { (head, _, tail) => ConsPattern(head, tail) }
  )
  
  lazy val commaPats: Parser[Seq[Pattern]] = (
      pat ^^ { p => Vector(p) }
    | commaPats ~ "," ~ pat ^^ { (ps, _, p) => ps :+ p }
  )
  
  lazy val num: Parser[Number] = (
      int    ^^ { i => IntL(i.toInt)       }
    | double ^^ { d => DoubleL(d.toDouble) }
  )
  
  lazy val id: Parser[Id] = 
    identifier ^^ { s => Id(Symbol(s)) }
    
  lazy val op: Parser[Op] = (
      "==" ^^ { s => Op(Symbol(s)) }
    | """\*|/""".r ^^ { s => Op(Symbol(s)) }
    | """\+|\-""".r ^^ { s => Op(Symbol(s)) }
  )
  /* lazy val op: Parser[Op] = 
    operator ^^ { s => Op(Symbol(s)) } */
  
  val identifier = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  
  val int = """0|-?[1-9]\d*"""r
  
  val double = """-?\d*.\d+"""r
  
  //val operator = """[=\+\*\-\?\\\$<>]+"""r
}
