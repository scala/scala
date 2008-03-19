// $Id$

package scala.util.parsing.combinator.testing

import scala.util.parsing.combinator._
import scala.util.parsing.input._

case class Ident(s: String)
case class Number(n: Int)
case class Str(s: String)

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
