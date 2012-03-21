package tweak.compiler 

import scala.util.parsing.combinator._
import tweak.compiler.ast._
import tweak.internal.types._
import tweak.util.StringUtil._

object TParser extends JavaTokenParsers {
    def interpret(s: String) = parseAll(all, s)
    
    def all: Parser[Any] = rep(expression | block)
    
    /* Block Production */
    def block: Parser[Any] = "{"~rep(expression)~"}"
    
    /* Expression Productions */
    def expression: Parser[Any] = (
      assignmentStatement 
    | ifStatement
    | variableDeclaration
    | operation 
    | value 
    )
    
    /* Assignment Productions */
    def assignmentStatement: Parser[Any] = (
      identifier~assignmentOp~value ^^ { case i~a~v => (i, v) }
    )
    
    def variableDeclaration: Parser[Any] = (
      ref~identifier~assignmentOp~value ^^ { 
        case "var"~i~a~v => new VarNode(i.asInstanceOf[IIdentifier], v) 
        case "val"~i~a~v => new ValNode(i.asInstanceOf[IIdentifier], v)
      }
    )
    
    def ref: Parser[Any] = (mutableRef | immutableRef)
    
    /* Value Production */
    def value: Parser[IType] = (
      boolean
    | double 
    | integer 
    | singleQuoteString 
    | doubleQuoteString
    //| function
    | symbol 
    | identifier  
    | list
    )
    
    /* If Statement Production */
    def ifStatement: Parser[Any] = "if"~"("~expression~")"~"then"~expression~opt("else"~(expression | ifStatement))~"end"

    /* Function Productions */
    def function: Parser[Any] = functionZero | functionN
    def functionZero: Parser[Any] = "Fn"~block
    def functionN: Parser[Any] = "Fn"~list~block
    /* application eventually without parens */
    def functionApplication: Parser[Any] = (
      opt("(")~>(identifier | function)~"apply"~repsep(value,",")<~opt(")"))  
    
    /* List Productions */
    def list: Parser[IList] = (
      "["~>repsep(value, ",")<~"]" ^^ {
        x => IList(x)
      }
    )
      
    def argumentList: Parser[IList] = ( 
      "["~>repsep(identifier, ",")<~"]" ^^ {
        x => new IList(x)
      }
    )
    
    
    /* Operator Productions */
    def operation: Parser[Any] = nestedOperation | singleOperation
    def nestedOperation: Parser[Any] = opt("(")~>singleOperation~operators~singleOperation<~opt(")")
    def singleOperation: Parser[Any] = opt("(")~>value~operators~value<~opt(")")
    def operators: Parser[Any] = additionOp | subtractionOp
    
    /* Terminals */
    //def comment: Parser[Any] = """--.*""".r ^^(x => None)
    def identifier: Parser[IType] = (
      """[a-zA-Z_]\w*""".r ^^ (x => IIdentifier(x))
    )
    
    def doubleQuoteString: Parser[IType] = (
      """(\".*\")""".r ^^ {
        x => IString(stripQuotes(x))
      }
    )
    
    def singleQuoteString: Parser[IType] = (
      "\'.*\'".r ^^ {
        x => IString(stripQuotes(x))
      }
    )
    
    def symbol: Parser[IType] = (
      """:.*""".r ^^ (x => ISymbol(Symbol(x)))
    )
    
    def integer: Parser[IType] = (
      wholeNumber ^^ (x => IInt(x.toInt))
    )
    
    def double: Parser[IType] = {
      """\d+\.\d+""".r ^^ {
        x => IDouble(x.toDouble)
      }
    }
    
    def boolean: Parser[IType] = (
      "true" ^^ (x => IBoolean(true)) 
    | "false" ^^ (x => IBoolean(false))
    )
    
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
