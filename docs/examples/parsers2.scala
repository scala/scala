package examples

object parsers2 {

  abstract class Tree
  case class Id(s: String)          extends Tree
  case class Num(n: int)            extends Tree
  case class Lst(elems: List[Tree]) extends Tree

  def isLetter = (c: char) => Character.isLetter(c)
  def isLetterOrDigit: char => boolean = Character.isLetterOrDigit
  def isDigit: char => boolean = Character.isDigit

  trait ListParsers extends CharParsers {

    def ident: Parser[Tree] =
      for (
        val c: char <- chr(isLetter);
        val cs: List[char] <- rep(chr(isLetterOrDigit))
      ) yield Id((c :: cs).mkString("", "", ""))

    def number: Parser[Tree] =
      for (
        val d: char <- chr(isDigit);
        val ds: List[char] <- rep(chr(isDigit))
      ) yield Num(((d - '0') /: ds) ((x, digit) => x * 10 + digit - '0'))

    def list: Parser[Tree] =
      for (
        val _ <- chr('(');
        val es <- listElems ||| succeed(List());
        val _ <- chr(')')
      ) yield Lst(es)

    def listElems: Parser[List[Tree]] =
      for (
        val x <- expr;
        val xs <- chr(',') &&& listElems ||| succeed(List())
      ) yield x :: xs

    def expr: Parser[Tree] =
      list ||| ident ||| number

  }

  class ParseString(s: String) extends Parsers {
    type inputType = int
    val input = 0
    def any = new Parser[char] {
      def apply(in: int): Parser[char]#Result =
        if (in < s.length()) Some(Pair(s charAt in, in + 1)) else None
    }
  }

  def main(args: Array[String]): unit =
    Console.println(
      if (args.length == 1) {
        val ps = new ParseString(args(0)) with ListParsers
        ps.expr(ps.input) match {
          case Some(Pair(list, _)) => "parsed: " + list
          case None => "nothing parsed"
        }
      }
      else
        "usage: scala examples.parsers2 <expr-string>"
    )

}
