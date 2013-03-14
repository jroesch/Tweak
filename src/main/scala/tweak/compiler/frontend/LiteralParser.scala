package tweak.compiler.frontend

import language.postfixOps
import com.codecommit.gll._
import tweak.compiler.ast._

trait LiteralParser extends TweakParser { //this: TweakParser => 

  lazy val literal: GLLParser[Literal] = (
    //  id
    //| op
    | string
    | num
  )

  val string: GLLParser[StringL]
  val num: GLLParser[Number]
  val tuple: GLLParser[TTuple]
  val list: GLLParser[TTuple]
}

trait DefaultLiteralParser extends LiteralParser {
  //this: TweakParser with LiteralParser =>

 lazy val string: GLLParser[String] =
    """"([^"]*)""""r
    
 lazy val num: GLLParser[Number] = (
      int    ^^ { i => IntL(i.toInt)       }
    | float  ^^ { d => DoubleL(d.toDouble) }
  )

  val int: GLLParser[String] = """0|-?[1-9]\d*"""r
  
  val float: GLLParser[String] = 
    ("""(\d+\.\d+((E|e)(\+|\-)?\d+)?(F|f|D|d)?)|""" +
     """(\.\d+((E|e)(\+|\-)?\d+)?(F|f|D|d)?)|""" +
     """(\d+((E|e)(\+|\-)?\d+)?(F|f|D|d))|""" + 
     """(\d+((E|e)(\+|\-)?\d+)(F|f|D|d)?)""").r
}
