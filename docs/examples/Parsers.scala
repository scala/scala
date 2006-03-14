package examples

abstract class Parsers {

  type inputType

  trait Parser[a] {

    type Result = Option[Pair[a, inputType]];

    def apply(in: inputType): Result;

    def filter(pred: a => boolean) = new Parser[a] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => if (pred(x)) Some(Pair(x, in1)) else None
      }
    }

    def map[b](f: a => b) = new Parser[b] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => Some(Pair(f(x), in1))
      }
    }

    def flatMap[b](f: a => Parser[b]) = new Parser[b] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => None
        case Some(Pair(x, in1)) => f(x).apply(in1)
      }
    }

    def ||| (p: => Parser[a]) = new Parser[a] {
      def apply(in: inputType): Result = Parser.this.apply(in) match {
        case None => p(in)
        case s => s
      }
    }

    def &&& [b](p: => Parser[b]): Parser[b] =
      for (val _ <- this; val x <- p) yield x
  }

  def succeed[a](x: a) = new Parser[a] {
    def apply(in: inputType): Result = Some(Pair(x, in))
  }

  def rep[a](p: Parser[a]): Parser[List[a]] =
    rep1(p) ||| succeed(List())

  def rep1[a](p: Parser[a]): Parser[List[a]] =
    for (val x <- p; val xs <- rep(p)) yield x :: xs

  def opt[a](p: Parser[a]): Parser[List[a]] =
    (for (val x <- p) yield List(x)) ||| succeed(List())
}

class Tokenizer(in: Iterator[char], delimiters: String) extends Iterator[String] {

  val EOI: char = 0;

  def nextChar() =
    if (in.hasNext) in.next else EOI;

  private var ch = nextChar();

  def isDelimiter(ch: Char) = {
    var i = 0;
    while (i < delimiters.length() && delimiters.charAt(i) != ch) { i = i + 1 }
    i < delimiters.length()
  }

  def hasNext: boolean = ch != EOI

  private val buf = new StringBuffer

  def next: String = {
    while (ch <= ' ' && ch != EOI) nextChar();
    if (ch == EOI) ""
    else {
      if (isDelimiter(ch)) ch.toString()
      else {
        buf.setLength(0); buf append ch
        while (ch > ' ' && ch != EOI && !isDelimiter(ch)) {
          buf append ch; nextChar();
        }
        buf.toString()
      }
    }
  }
}

trait TokenParsers extends Parsers {
  type inputType = Stream[String]
  def nextToken() = new Parser[String] {
    def apply(in: inputType): Result =
      if (in.isEmpty) None else Some(Pair(in.head, in.tail))
  }
}

trait CharParsers extends Parsers {
  def any: Parser[char]
  def chr(ch: char) =
    for (val c <- any; c == ch) yield c
  def chr(p: char => boolean) =
    for (val c <- any; p(c)) yield c
}
