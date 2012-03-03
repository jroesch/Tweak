import scala.util.parsing.combinator._
import tweak.lang.compiler._

object TweakParser extends JavaTokenParsers {
    def value: Parser[Any] = integer | double | symbol | string | identifier | function
    def list: Parser[Any] = "["~repsep(value, ",")~"]"
    //def classDef: Parser[Any] = "class"~block
    //def obj: Parser[Any] = "object"~block
    def block: Parser[Any] = "{"~rep(value)~"}"
    //def expr: Parser[Any] = value
    
    /* Terminals and Simple Productions */
    
    //def ~identifier~assignment~value~"\n"
    def ref: Parser[Any] = (mutableRef | immutableRef)
    def mutableRef: Parser[Any] = "var"
    def immutableRef: Parser[Any] = "val"
    def function: Parser[Any] = functionZero | functionN
    def functionZero: Parser[Any] = "Fn"~block
    def functionN: Parser[Any] = "Fn"~list~block
    def nonEqualOp: Parser[Any] = """[<>?:!@#$%\^&\*\-+]+""".r
    def equalOp: Parser[Any] = """=*[=<>?:!@#$%\^&\*\-+]+""".r
    def assignmentOp: Parser[Any] = "="
    
    def identifier: Parser[String] = """[a-zA-Z_]\w*""".r 
    def string: Parser[String] = """[\"\'].*[\"\']""".r
    def symbol: Parser[String] = """:.*""".r
    def integer: Parser[Any] = wholeNumber
    def double: Parser[Any] = floatingPointNumber
    def operator: Parser[Any] = nonEqualOp | equalOp  
}


