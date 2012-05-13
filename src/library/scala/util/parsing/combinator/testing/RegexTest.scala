
package scala.util.parsing.combinator.testing

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import language.postfixOps

@deprecated("This class will be removed", "2.10.0")
case class Ident(s: String)
@deprecated("This class will be removed", "2.10.0")
case class Number(n: Int)
@deprecated("This class will be removed", "2.10.0")
case class Str(s: String)

@deprecated("This class will be removed", "2.10.0")
object RegexTest extends RegexParsers {
  val ident: Parser[Any] = """[a-zA-Z_]\w*""".r ^^ (s => Ident(s))
  val number: Parser[Any] = """\d\d*""".r ^^ (s => Number(s.toInt))
  val string: Parser[Any] = "\".*\"".r ^^ (s => Str(s.substring(1, s.length - 1)))
  val parser = (ident | number | string)*

  def main(args: Array[String]) = {
    val in = args mkString " "
    println("\nin : "+in)
    println(phrase[Any](parser)(new CharSequenceReader(in)))
  }
}
