package tweak.util

import scala.annotation._ 

object StringUtil {
  //@tailrec TODO: rewrite to be iterative
  def join[T](items: Seq[T], sep: String): String = {
    if (items.tail.isEmpty)
      return items.head.toString
    else 
      return items.head.toString + sep + join(items.tail, sep)
  }
  
  def stripQuotes(string: String) = string.substring(1, string.length - 1)
}