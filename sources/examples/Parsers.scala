package examples;

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
