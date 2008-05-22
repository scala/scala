import java.io.{File,StringReader}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{CharArrayReader, StreamReader}

class TestParsers extends Parsers {
  type Elem = Char

  def p: Parser[List[Int]] = rep(p1 | p2)
  def p1: Parser[Int] = 'a' ~ nl ~ 'b' ~ nl ^^^ 1
  def p2: Parser[Int] = 'a' ~ nl ^^^ 2
  def nl: Parser[Int] = rep(accept('\n') | accept('\r')) ^^^ 0
}

object Test {
  def main(args: Array[String]): Unit = {
    val tstParsers = new TestParsers
    val s = "a\na\na"
    val r1 = new CharArrayReader(s.toCharArray())
    val r2 = StreamReader(new StringReader(s))
    println(tstParsers.p(r1))
    println(tstParsers.p(r2))
  }
}
