package tweak.compiler 

import scala.util.parsing.combinator._
import tweak.compiler.ast._

object TParser extends JavaTokenParsers {
    def all: Parser[Any] = rep(expression | block)
    
    /* Block Production */
    def block: Parser[Any] = "{"~rep(expression)~"}"
    
    /* Expression Productions */
    def expression: Parser[Any] = assignmentExpression | complexOperation | operation | value 
    
    /* Assignment Productions */
    def assignmentExpression: Parser[Any] = ref~identifier~assignmentOp~value
    def ref: Parser[Any] = (mutableRef | immutableRef)
    
    /* Value Production */
    def value: Parser[Any] = (
      boolean
    | integer 
    | double 
    | singleQuoteString 
    | doubleQuoteString
    | function
    | symbol 
    | identifier  
    | list
    )
    /* If Statement Production */
    //def ifStatement: Parser[Any] = "if"~condition~"then"~expression~opt("else"~expression~)~"end"
    /* work on nested operations */
    def complexOperation: Parser[Any] = opt("(")~(complexPrefixOperation | complexInfixOperation)~opt(")")
    
    def complexPrefixOperation: Parser[Any] = wrappedOperator~repsep(operation | value, ",")
    
    def complexInfixOperation: Parser[Any] = (operation | value)~operator~repsep(operation | value, ",")
    
    def operation : Parser[Any] = (
      prefixOperation 
    | infixOperation 
    | functionApplication
    )
    
    /* Function Productions */
    def function: Parser[Any] = functionZero | functionN
    def functionZero: Parser[Any] = "Fn"~block
    def functionN: Parser[Any] = "Fn"~list~block
    /* application eventually without parens */
    def functionApplication: Parser[Any] = (
      "("~(identifier | function)~"apply"~repsep(value,",")~")"
    | (identifier | function)~"apply"~repsep(value,","))
    
    /* List Productions */
    def list: Parser[Any] = "["~>repsep(value, ",")<~"]" ^^ (x => new ListNode(x))
    def argumentList: Parser[Any] = "["~>repsep(identifier, ",")<~"]" ^^ (x => new ArgListNode(x))
    
    /* Operator Productions */
    def prefixOperation: Parser[Any] = "("~operator~")"~repsep(value, ",")
    def infixOperation: Parser[Any] = value~operator~repsep(value, ",")
    def wrappedOperator: Parser[Any] = "("~operator~")"
    def operator: Parser[Any] = nonEqualOp | equalOp
    
    /* Terminals */
    //def comment: Parser[String] = """--.*""".r ^^(x => Nothing)
    def identifier: Parser[String] = """[a-zA-Z_]\w*""".r 
    def doubleQuoteString: Parser[String] = """(\".*\")""".r
    def singleQuoteString: Parser[String] = "\'.*\'".r
    def symbol: Parser[String] = """:.*""".r
    def integer: Parser[Any] = wholeNumber
    def double: Parser[Any] = floatingPointNumber
    def boolean: Parser[Any] = "true" ^^ (x => true) | "false" ^^ (x => false)
    def nonEqualOp: Parser[Any] = """[<>?:!@#$%\^&\*\-+]+""".r
    def equalOp: Parser[Any] = """=*[=<>?:!@#$%\^&\*\-+]+""".r
    def assignmentOp: Parser[Any] = "="
    def mutableRef: Parser[Any] = "var"
    def immutableRef: Parser[Any] = "val"
}


