package tweak.compiler.frontend

import com.codecommit.gll._
import tweak.compiler.ast._
import tweak.internal.types._
import tweak.util.StringUtil._
import language.postfixOps

object Parser extends RegexParsers { 
  type GLLParser[T] = Parser[T]

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
  
  lazy val fun: GLLParser[TFunction] = 
    matcher ^^ { ms => TFunction(ms) }
    
  lazy val exp: GLLParser[Exp] = (
      id
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ null
    | num
    | op 
    | fun
    | exp ~ exp ^^ { (f, g) => ApplyStream.genStream(f, g) }
    | "nil" ^^ null
    | exp ~ "::" ~ exp ^^ null
    | "case" ~ exp ~ "of" ~ matcher ^^ null
    | "let" ~ decs ~ "in" ~ exp ~ "end" ^^ null
    | "(" ~ ")" ^^ { (_, _) => UnitL }
    | "(" ~ exp ~ ")" ^^ { (_, e, _) => e }
    | "(" ~ commaExps ~ ")" ^^ null
  )
  
  lazy val commaExps: GLLParser[Any] = 
    (exp ~ "," ~ exp) ~ (("," ~ exp)*) ^^ null
  
  lazy val matcher: GLLParser[Seq[Match]] = (
      mrule ^^ { m => Vector(m) }
    | mrule ~ "|" ~ matcher ^^ { (m, _, ms) => m +: ms }
  )
  
  lazy val mrule = pat ~ "=>" ~ exp ^^ { (pat, _, exp) => Match(pat, exp) }
          
  lazy val pat: GLLParser[Pattern] = (
      "_" ^^ { _ => WildCardPattern }
    | id  ^^ { ident => IdPattern(ident) }
    | num ^^ { n => LiteralPattern(n) }
    | "(" ~ ")" ^^ { (_, _) => LiteralPattern(UnitL) }
    | "(" ~ commaPats ~ ")" ^^ { (_, ps, _) => PatternSeq(ps) }
    //| "nil"
    | pat ~ "::" ~ pat ^^ { (head, _, tail) => ConsPattern(head, tail) }
  )
  
  lazy val commaPats: GLLParser[Seq[Pattern]] = (
      pat ^^ { p => Vector(p) }
    | commaPats ~ "," ~ pat ^^ { (ps, _, p) => ps :+ p }
  )
  
  lazy val num: GLLParser[Number] = (
      int    ^^ { i => IntL(i.toInt)       }
    | double ^^ { d => DoubleL(d.toDouble) }
  )
  
  lazy val id: GLLParser[Id] = 
    identifier ^^ { s => Id(Symbol(s)) }
    

  lazy val op: GLLParser[Op] = 
    operator ^^ { s => Op(Symbol(s)) }
  
  val identifier = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  
  val int = """0|-?[1-9]\d*"""r
  
  val double = """-?\d*.\d+"""r
  
  val operator = """[=\+\*\-\?\\\$<>]+"""r
}
