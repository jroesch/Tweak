package tweak.compiler 

import scala.util.parsing.combinator._
import tweak.compiler.ast._
import tweak.util.ParseUtil._

object TParser extends JavaTokenParsers {
    def interpret(s: String) = parseAll(all, s)
    
    def all: Parser[Any] = rep(expression | block)
    
    /* Block Production */
    def block: Parser[Any] = "{"~rep(expression)~"}"
    
    /* Expression Productions */
    def expression: Parser[Any] = (
      assignmentExpression //^^ (x => buildAssignmentNode(x))
    | operation 
    | value 
    )
    
    /* Assignment Productions */
    def assignmentExpression: Parser[Any] = ref~identifier~assignmentOp~value
    def ref: Parser[Any] = (mutableRef | immutableRef)
    
    /* Value Production */
    def value: Parser[Any] = (
      boolean
    | double 
    | integer 
    | singleQuoteString 
    | doubleQuoteString
    | function
    | symbol 
    | identifier  
    | list
    )
    
    /* If Statement Production */
    //def ifStatement: Parser[Any] =     "if"~condition~"then"~expression~opt("else"~expression~)~"end"

    /* Function Productions */
    def function: Parser[Any] = functionZero | functionN
    def functionZero: Parser[Any] = "Fn"~block
    def functionN: Parser[Any] = "Fn"~list~block
    /* application eventually without parens */
    def functionApplication: Parser[Any] = (
      opt("(")~>(identifier | function)~"apply"~repsep(value,",")<~opt(")"))  
    
    /* List Productions */
    def list: Parser[Any] = "["~>repsep(value, ",")<~"]" //^^ (x => new ListNode(x))
    def argumentList: Parser[Any] = "["~>repsep(identifier, ",")<~"]" ^^ (x => new ArgListNode(x))
    
    
    /* Operator Productions */
    def operation: Parser[Any] = nestedOperation | singleOperation
    def nestedOperation: Parser[Any] = opt("(")~>singleOperation~operators~singleOperation<~opt(")")
    def singleOperation: Parser[Any] = opt("(")~>value~operators~value<~opt(")")
    def operators: Parser[Any] = additionOp | subtractionOp
    
    /* Terminals */
    //def comment: Parser[Any] = """--.*""".r ^^(x => None)
    def identifier: Parser[Any] = """[a-zA-Z_]\w*""".r //resolve the identifer 
    def doubleQuoteString: Parser[Any] = """(\".*\")""".r ^^ (x => newStringNode(x))
    def singleQuoteString: Parser[Any] = "\'.*\'".r ^^ (x => newStringNode(x))
    def symbol: Parser[Any] = """:.*""".r ^^ (x => newSymbolNode(x))
    def integer: Parser[Any] = wholeNumber ^^ (x => newIntegerNode(x))
    def double: Parser[Any] = """\d+\.\d+""".r ^^ (x => newDoubleNode(x))
    def boolean: Parser[Any] = "true" ^^ (x => newBooleanNode(true)) | "false" ^^ (x => newBooleanNode(false))
    def assignmentOp: Parser[Any] = "="
    def additionOp: Parser[Any] = "+"
    def subtractionOp: Parser[Any] = "-"
    def mutableRef: Parser[Any] = "var"
    def immutableRef: Parser[Any] = "val"
}

/* in-dev rules 
def nonEqualOp: Parser[Any] = """[<>?:!@#$%\^&\*\-+]+""".r
def equalOp: Parser[Any] = """=*[=<>?:!@#$%\^&\*\-+]+""".r

def prefixOperation: Parser[Any] = "("~operator~")"~repsep(value, ",")
def infixOperation: Parser[Any] = value~operator~repsep(value, ",")
def wrappedOperator: Parser[Any] = "("~operator~")"
def operator: Parser[Any] = nonEqualOp | equalOp

def operation : Parser[Any] = (
  prefixOperation 
| infixOperation 
| functionApplication
)
*/
