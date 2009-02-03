
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.CharArrayReader

object TestJavaTokenParsers extends JavaTokenParsers {
}

object Test {
  import TestJavaTokenParsers._

  def main(args : Array[String]) {
    println(decimalNumber(new CharArrayReader("1.1".toCharArray)))
    println(decimalNumber(new CharArrayReader("1.".toCharArray)))
    println(decimalNumber(new CharArrayReader(".1".toCharArray)))
    println(decimalNumber(new CharArrayReader("!1".toCharArray)))
  }
}
