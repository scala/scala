import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

class TestParsers extends Parsers {
  type Elem = Char

  def p: Parser[List[Char]] = rep1(p1)
  def p1: Parser[Char] = accept('a') | err("errors are propagated")
}

object Test {
  def main(args: Array[String]): Unit = {
    val tstParsers = new TestParsers
    val s = new CharSequenceReader("aaab")
    println(tstParsers.p(s))
  }
}
