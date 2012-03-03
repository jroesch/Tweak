package tweak.compiler 

import scala.util.parsing.combinator._

object TweakParser extends JavaTokenParsers {
    def test: Parser[Any] = rep(value) 
    //def classDef: Parser[Any] = "class"~block
    //def obj: Parser[Any] = "object"~block
    def block: Parser[Any] = "{"~rep(value)~"}"
    def expression: Parser[Any] = (
      value
    | list
    )
    
    /* Assignment Productions */
    def assignmentExpr: Parser[Any] = ref~identifier~assignmentOp~value
    def ref: Parser[Any] = (mutableRef | immutableRef)

    /* Value Production */
    def value: Parser[Any] = (
      integer 
    | double 
    | symbol 
    | singleQuoteString 
    | doubleQuoteString
    | identifier  
    | list
    | function
    )
    
    /* Function Productions */
    def function: Parser[Any] = functionZero | functionN
    def functionZero: Parser[Any] = "Fn"~block
    def functionN: Parser[Any] = "Fn"~list~block
    
    /* List Productions */
    def list: Parser[Any] = "["~repsep(value, ",")~"]"
    def argumentList: Parser[Any] = "["~repsep(identifier, ",")~"]"
  
    /* Operator Productions */
    def operation : Parser[Any] = prefixOperation | infixOperation 
    def prefixOperation: Parser[Any] = "("~operator~")"~expr~expr
    def infixOperation: Parser[Any] = expr~operator~expr
    def wrappedOperator: Parser[Any] = "("~operator~")"
    def operator: Parser[Any] = nonEqualOp | equalOp
    
    /* Terminals */
    def identifier: Parser[String] = """[a-zA-Z_]\w*""".r 
    def doubleQuoteString: Parser[String] = """\".*\""".r
    def singleQuoteString: Parser[String] = "\'.*\'".r
    def symbol: Parser[String] = """:.*""".r
    def integer: Parser[Any] = wholeNumber
    def double: Parser[Any] = floatingPointNumber
    def nonEqualOp: Parser[Any] = """[<>?:!@#$%\^&\*\-+]+""".r
    def equalOp: Parser[Any] = """=*[=<>?:!@#$%\^&\*\-+]+""".r
    def assignmentOp: Parser[Any] = "="
    def mutableRef: Parser[Any] = "var"
    def immutableRef: Parser[Any] = "val"
}


