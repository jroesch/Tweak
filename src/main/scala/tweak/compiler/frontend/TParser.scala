package tweak.compiler.frontend

import com.codecommit.gll._
import Parsers._
import RegexParsers._
import tweak.compiler.ast._
import tweak.internal.types._
import tweak.util.StringUtil._
import language.postfixOps

object TParser extends RegexParsers {
  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r
  
  // %%
  
  lazy val program = decs
  
  lazy val decs = dec*
  
  lazy val dec = (
      "val" ~ pat ~ "=" ~ exp ^^ { (_, p, _, e) => Binding(pat = p, exp = e) }
    //| "val" ~ id ~ "=" ~ "fn" ~ matcher ^^ null
  )
  
  lazy val exp: Parser[Exp] = (
      id ^^ { s => Id(Symbol(s)) }
    //| b
    | "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp
    | num
    | exp ~ op ~ exp
    
    | "(" ~ ")"
    | "(" ~ commaExps ~ ")"
    | "fn" ~ matcher
    | exp ~ exp
    
    | "nil"
    | exp ~ "::" ~ exp
    | "case" ~ exp ~ "of" ~ matcher
    | "let" ~ decs ~ "in" ~ exp ~ "end"
  ) ^^^ null
  
  lazy val commaExps: Parser[Any] = (
      exp
    | commaExps ~ "," ~ exp
  )
  
  lazy val matcher: Parser[Any] = (
      mrule
    | mrule ~ "|" ~ matcher
  ) ^^^ null
  
  lazy val mrule = pat ~ "=>" ~ exp ^^^ null
          
  lazy val pat: Parser[Pattern] = (
      "_"
    | id ^^ { s => Id(Symbol(s)) }
    //| b
    | num
    | "(" ~ ")"
    | "(" ~ commaPats ~ ")"
    | "nil"
    | pat ~ "::" ~ pat
  ) ^^^ null
  
  lazy val commaPats: Parser[Any] = (
      pat
    | commaPats ~ "," ~ pat
  ) ^^^ null
  
  lazy val num: Parser[Number] = (
      int    ^^ { i => IntL(i.toInt)       }
    | double ^^ { d => DoubleL(d.toDouble) }
  )
  val id = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  
  //val b = "true" | "false"
  
  val int = """0|-?[1-9]\d*"""r
  
  val double = """-?\d*.\d+"""r
  
  val op = (
      "=" | "<" | ">" | "<=" | ">=" 
    | "+" | "-" | "*" | "div" | "mod" 
    | "@" | "o" | "andalso" | "orelse"
  )
}
