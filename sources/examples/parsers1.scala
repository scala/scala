package examples;

object parsers1 {
/*
abstract class Parsers {

  type intype;

  trait Parser[a] {

    type Result = Option[Pair[a, intype]];

    def apply(in: intype): Result;

    def filter(pred: a => boolean) = new Parser[a] {
      def apply(in: intype): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => if (pred(x)) Some(Pair(x, in1)) else None
      }
    }

    def map[b](f: a => b) = new Parser[b] {
      def apply(in: intype): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => Some(Pair(f(x), in1))
      }
    }

    def flatMap[b](f: a => Parser[b]) = new Parser[b] {
      def apply(in: intype): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => f(x).apply(in1)
      }
    }

    def ||| (def p: Parser[a]) = new Parser[a] {
      def apply(in: intype): Result = Parser.this.apply(in) match {
	case None => p(in)
	case s => s
      }
    }

    def &&& [b](def p: Parser[b]): Parser[b] =
      for (val _ <- this; val x <- p) yield x;
  }

  def succeed[a](x: a) = new Parser[a] {
    def apply(in: intype): Result = Some(Pair(x, in))
  }

  def rep[a](p: Parser[a]): Parser[List[a]] =
    rep1(p) ||| succeed(List());

  def rep1[a](p: Parser[a]): Parser[List[a]] =
    for (val x <- p; val xs <- rep(p)) yield x :: xs;

  def opt[a](p: Parser[a]): Parser[List[a]] =
    (for (val x <- p) yield List(x)) ||| succeed(List());
}

abstract class CharParsers extends Parsers {
  def any: Parser[char];
  def chr(ch: char) =
    for (val c <- any; c == ch) yield c;
  def chr(p: char => boolean) =
    for (val c <- any; p(c)) yield c;
}
*/
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
      } else "usage: java examples.Test <expr-string>"
    );
}

