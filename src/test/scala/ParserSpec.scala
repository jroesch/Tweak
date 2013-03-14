import org.specs2.mutable._


import tweak.compiler.frontend.{ Parser => P }
import tweak.compiler.{ ast => terms }
import com.codecommit.gll._
import scala.collection.mutable

object ParserSpec extends Specification {

 // implicit class ResultW[R <: Term](val r: Stream[Result[R]]) extends AnyVal {
 //   def result: Term = r match {
 //     case Stream(h, _) => h match {
 //     case _ => new Exception("

  "The int parser" should {
    "correctly parse integers" in {
      val ints = List("0", "1", "20", "34000", "45676940")
      ints map { i =>
        P.int(i) must beLike {
          case Success(ip, LineStream()) #:: Stream.Empty if i == ip => ok 
        }
      } 
    }

    "fail to parse non-integers" in {
      val badInts = List("01", "1.11", "5n10", "0b1546")
      badInts map { i => 
        P.int(i) must beLike {
          case Failure(_, _) #:: Stream.Empty => ok
        }
      }
    }
  }

  "The float parser" should {
    "correctly parse floats" in {
      val floats = List("0.1", "0.0", ".1", 
                         "2.100", "2e10", "9E50",
                         "1.0e5", "2.05E5", "100e4")
      floats map { d => 
        P.float(d) must beLike {
          case Success(dp, LineStream()) #:: Stream.Empty if d == dp => ok
        }
      }
    }

  "fail to parse non-floats" in {
    val badFloats = List("1", "string", "2000", "id", "2c10")

      badFloats map { d =>
        P.float(d) must beLike {
          case Failure(_, _) #:: Stream.Empty => ok
        }
      }
    }
  }
  
  "The string parser" should {
    "parse a simple string" in {
      val str = """"Hello World!"""
      P.string(str) === StringL("Hello World")
    }
  }

  "The exp parser should" should {
    "parse () as UnitL" in {
      P.exp("()") === terms.UnitL
    }
    
    "parse a num as either a DoubleL or IntL" in {
      P.exp("1") must beLike { case i: terms.IntL => ok }
      P.exp("2.00") must beLike { case d: terms.DoubleL => ok }
    }
    
    "parse an identifier as a Id" in {
      P.exp("an_id") must beLike { case id: terms.Id => ok }
    }
  }
}
