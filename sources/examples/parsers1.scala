package examples;

trait Tree {}
case class Var(n: String)                   : Tree extends Tree {}
case class Num(n: Int)                      : Tree extends Tree {}
case class Binop(op: Char, l: Tree, r: Tree): Tree extends Tree {}

module Parse {

  type Result[b] = Option[Pair[b, List[Char]]];

  trait Parser[p] extends Function1[List[Char], Result[p]] {

    def apply(in: List[Char]): Result[p];

    def filter(p: p => Boolean) = new Parser[p] {
      def apply(in: List[Char]): Result[p] = Parser.this.apply(in) match {
        case Some(Pair(x, in1)) => if (p(x)) Some(Pair(x, in1)) else None()
        case n => n
      }
    }

    def map[b](f: p => b) = new Parser[b] {
      def apply(in: List[Char]): Result[b] = Parser.this.apply(in) match {
	case Some(Pair(x, in1)) => Some(Pair(f(x), in1))
        case None() => None()
      }
    }

    def flatMap[b](f: p => Parser[b]) = new Parser[b] {
      def apply(in: List[Char]): Result[b] = Parser.this.apply(in) match {
	case Some(Pair(x, in1)) => f(x)(in1)
        case None() => None()
      }
    }

    def ||| (def p: Parser[p]) = new Parser[p] {
      def apply(in: List[Char]): Result[p] = Parser.this.apply(in) match {
	case None() => p(in)
	case s => s
      }
    }

    def &&& [b](def p: Parser[b]): Parser[b] =
      for (val _ <- this; val result <- p) yield result;
  }

  def succeed[p](x: p) = new Parser[p] {
    def apply(in: List[Char]) = Some(Pair(x, in))
  }

  def rep[a](p: Parser[a]): Parser[List[a]] =
    rep1(p) ||| succeed(List());

  def rep1[a](p: Parser[a]): Parser[List[a]] =
    for (val x <- p; val xs <- rep(p)) yield x :: xs;

  def opt[a](p: Parser[a]): Parser[Option[a]] =
    (for (val x <- p) yield Some(x): Option[a]) ||| succeed(None(): Option[a]);
}


module ExprParser {
  import Parse._;

  def chrWith(p: Char => Boolean) = new Parser[Char] {
    def apply(in: List[Char]): Result[Char] = in match {
      case List() => None()
      case (c :: in1) => if (p(c)) Some(Pair(c, in1)) else None()
    }
  }

  def chr(c: Char): Parser[Char] = chrWith(d => d == c);

  def letter: Parser[Char] = chrWith(c => c.isLetter);
  def digit : Parser[Char] = chrWith(c => c.isDigit);

  def ident: Parser[String] =
    for (val c <- letter; val cs <- rep(letter ||| digit))
    yield ((c :: cs) foldr "") {(c, s) => c+ s}

  def number: Parser[Int] =
    for (val d <- digit; val ds <- rep(digit))
    yield ((d - '0') foldl_: ds) {(x, y) => x * 10 + (y - '0')};

  def expr: Parser[Tree] =
    for {
      val e1 <- expr1;
      val es <- rep (
	for {
	  val op <- chr('+') ||| chr('-');
	  val e <- expr1
	} yield (x: Tree => Binop(op, x, e))
      )
    } yield (e1 foldl_: es) {(x,f) => f(x)}

  def expr1: Parser[Tree] =
    for {
      val e1 <- expr2;
      val es <- rep (
	for {
	  val op <- chr('*') ||| chr('/');
	  val e <- expr2
	} yield (x: Tree => Binop(op, x, e))
      )
    } yield (e1 foldl_: es) {(x,f) => f(x)}

  def expr2: Parser[Tree] =
        (for { val n <- ident } yield Var(n))
    ||| (for { val n <- number } yield Num(n))
    ||| (for { val _ <- chr('('); val e <- expr; val _ <- chr(')') } yield e);

  private def applyAll[a](fs: List[a => a], x: a) =
    (x foldl_: fs) { (x, f) => f(x) }
}
