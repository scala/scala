package examples;

abstract class Parsers {

  type input;

  trait Parser[a] {

    type Result = Option[Pair[a, input]];

    def apply(in: input): Result;

    def filter(pred: a => boolean) = new Parser[a] {
      def apply(in: input): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => if (pred(x)) Some(Pair(x, in1)) else None
      }
    }

    def map[b](f: a => b) = new Parser[b] {
      def apply(in: input): Result = Parser.this.apply(in) match {
        case None => None
	case Some(Pair(x, in1)) => Some(Pair(f(x), in1))
      }
    }

    def flatMap[b](f: a => Parser[b]) = new Parser[b] {
      def apply(in: input): Result = Parser.this.apply(in) match {
        case None => None
	case Some(Pair(x, in1)) => f(x).apply(in1)
      }
    }

    def ||| (def p: Parser[a]) = new Parser[a] {
      def apply(in: input): Result = Parser.this.apply(in) match {
	case None => p(in)
	case s => s
      }
    }

    def &&& [b](def p: Parser[b]): Parser[b] =
      for (val _ <- this; val x <- p) yield x;
  }

  def succeed[a](x: a) = new Parser[a] {
    def apply(in: input) = Some(Pair(x, in))
  }

  def rep[a](p: Parser[a]): Parser[List[a]] =
    rep1(p) ||| succeed(List());

  def rep1[a](p: Parser[a]): Parser[List[a]] =
    for (val x <- p; val xs <- rep(p)) yield x :: xs;

  def opt[a](p: Parser[a]): Parser[Option[a]] =
    (for (val x <- p) yield List(x)) ||| List();
}

abstract class ExprParsers extends Parsers {

  def any: Parser[char];

  def chr(ch: char) =
    for (val c <- any; c == ch) yield c;

  def chr(p: char => boolean) =
    for (val c <- any; p(c)) yield c;

  def ident: Parser[String] =
    for (
      val c: char <- chr(Character.isLetter);
      val cs: List[char] <- rep(chr(Character.isLetterOrDigit))
    ) yield (c :: cs).mkString("", "", "");

  def number: Parser[int] =
    for (
      val d: char <- chr(Character.isDigit);
      val ds: List[char] <- rep(chr(Character.isDigit))
    ) yield ((d - '0') /: ds) ((x, digit) => x * 10 + digit - '0');

  def list: Parser[List[Tree]] =
    for (
      val _ <- chr('(');
      val es <- listElems ||| succeed(List());
      val _ <- chr(')')
    ) yield es;

  def listElems: Parser[List[Any]] =
    for (
      val x <- expr;
      val xs <- chr(',') &&& listElems ||| succeed(List())
    ) yield x :: xs;

  def expr: Parser[Any] =
    list ||| ident ||| number;

}

object Test {
  def main(args: Array[String]) =
    System.out.println(
      if (args.length == 1)
	new ExprParserFamily(args(0)).expr(0) match {
	  case Some(Pair(tree, _)) => tree.toString();
	  case None => "Syntax error"
	}
      else "usage: java examples.Test <expr-string>"
    );
}

