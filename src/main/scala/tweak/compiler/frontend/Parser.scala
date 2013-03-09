package tweak.compiler.frontend

import com.codecommit.gll._
import tweak.compiler.ast._
import tweak.internal.types._
import tweak.util.StringUtil._
import language.postfixOps

object Parser extends RegexParsers { 
  type GLLParser[T] = Parser[T]
  
  /* implicit class UnwrapResult[A <: Term](val r: Result[A]) extends AnyVal {
    def result: Term = r match {
      case Success
    }
  } */

  override val whitespace = """(\s|\(\*([^*]|\*[^)])*\*\))+"""r
  
  lazy val interpreter = exp | dec
  
  lazy val program: GLLParser[Program] = (dec*) ^^ { ds => Program(ds) }
  
  lazy val decs = dec*
  
  lazy val dec: GLLParser[Binding] = (
      "val" ~ pat ~ "=" ~ exp ^^ { (_, p, _, e) => Binding(pat = p, exp = e) }
    | "val" ~ id ~ "=" ~ fun ^^ { 
      (_, ident, _, f) => Binding(pat = IdPattern(ident), exp = f) 
    }
  )
  
  lazy val fun: GLLParser[TFunction] = 
    matcher ^^ { ms => TFunction(ms) }
    
  lazy val exp: GLLParser[Exp] = (
      id
    //| "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp ^^ null
    | num
    | op 
    | fun
    | exp ~ exp ^^ { (f, g) => ApplyStream.genStream(f, g) }
    | "nil" ^^ null
    | exp ~ "::" ~ exp ^^ null
    | "case" ~ exp ~ "of" ~ matcher ^^ null
    | "let" ~ decs ~ "in" ~ exp ~ "end" ^^ null
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
    | float  ^^ { d => DoubleL(d.toDouble) }
  )
  
  lazy val id: GLLParser[Id] = 
    identifier ^^ { s => Id(Symbol(s)) }
    

  lazy val op: GLLParser[Op] = 
    operator ^^ { s => Op(Symbol(s)) }
  
  val identifier: GLLParser[String] = """[a-zA-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*"""r
  
  val int: GLLParser[String] = """0|-?[1-9]\d*"""r
  
  val float: GLLParser[String] = 
    ("""(\d+\.\d+((E|e)(\+|\-)?\d+)?(F|f|D|d)?)|""" +
     """(\.\d+((E|e)(\+|\-)?\d+)?(F|f|D|d)?)|""" +
     """(\d+((E|e)(\+|\-)?\d+)?(F|f|D|d))|""" + 
     """(\d+((E|e)(\+|\-)?\d+)(F|f|D|d)?)""").r

  
  val operator: GLLParser[String] = """[=\+\*\-\?\\\$<>]+"""r
  
  def mkErrorMsg(failed: Failure) = {
    val Failure(data, tail) = failed
    val expected = data match {
      case ExpectedRegex(regex) => s"Expected the_regex on "
    }
  }
}
