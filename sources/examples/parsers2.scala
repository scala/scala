package examples;

object parsers2 {

  abstract class Tree{}
  case class Id (s: String)         extends Tree {}
  case class Num(n: int)            extends Tree {}
  case class Lst(elems: List[Tree]) extends Tree {}

  abstract class ListParsers extends CharParsers {

    def ident: Parser[Tree] =
      for (
        val c: char <- chr(Character.isLetter);
        val cs: List[char] <- rep(chr(Character.isLetterOrDigit))
      ) yield Id((c :: cs).mkString("", "", ""));

    def number: Parser[Tree] =
      for (
        val d: char <- chr(Character.isDigit);
        val ds: List[char] <- rep(chr(Character.isDigit))
      ) yield Num(((d - '0') /: ds) ((x, digit) => x * 10 + digit - '0'));

    def list: Parser[Tree] =
      for (
        val _ <- chr('(');
        val es <- listElems ||| succeed(List());
        val _ <- chr(')')
      ) yield Lst(es);

    def listElems: Parser[List[Tree]] =
      for (
        val x <- expr;
        val xs <- chr(',') &&& listElems ||| succeed(List())
      ) yield x :: xs;

    def expr: Parser[Tree] =
      list ||| ident ||| number;

  }

  class ParseString(s: String) extends Parsers {
    type intype = int;
    val input = 0;
    def any = new Parser[char] {
      def apply(in: int): Parser[char]#Result =
        if (in < s.length()) Some(Pair(s charAt in, in + 1)) else None;
    }
  }

  def main(args: Array[String]): unit =
    Console.println(
      if (args.length == 1) {
        val ps = new ListParsers with ParseString(args(0));
        ps.expr(ps.input) match {
          case Some(Pair(list, _)) => Console.println("parsed: " + list);
          case None => "nothing parsed"
        }
      } else "usage: java examples.parsers2 <expr-string>"
    );

}

