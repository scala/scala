package examples;

module Parse {

  type Result = Option[List[Char]];

  trait Parser with Function1[List[Char],Result] {
    def &&& (def p: Parser) = new Parser {
      def apply(in: List[Char]) = Parser.this.apply(in) match {
        case Some(in1) => p(in1)
        case n => n
      }
    }

    def ||| (def p: Parser) = new Parser {
      def apply(in: List[Char]) = Parser.this.apply(in) match {
        case None() => p(in)
        case s => s
      }
    }
  }

  val empty = new Parser { def apply(in: List[Char]): Result = Some(in) }

  def fail = new Parser { def apply(in: List[Char]): Result = None() }

  def chrWith(p: Char => Boolean) = new Parser {
    def apply(in: List[Char]): Result = in match {
      case List() => None()
      case (c :: in1) => if (p(c)) Some(in1) else None()
    }
  }

  def chr(c: Char): Parser = chrWith(d => d == c);

  def opt(p: Parser): Parser = p ||| empty;
  def rep(p: Parser): Parser = opt(rep1(p));
  def rep1(p: Parser): Parser = p &&& rep(p);
}

module ExprParser {
  import Parse._;

  def letter    =   chrWith(c => c.isLetter);
  def digit     =   chrWith(c => c.isDigit);

  def ident     =   letter &&& rep(letter ||| digit);
  def number    =   digit &&& rep(digit);

  def expr:Parser =  expr1 &&& rep((chr('+') &&& expr1) ||| (chr('-') &&& expr1));
  def expr1     =  expr2 &&& rep((chr('*') &&& expr2) ||| (chr('/') &&& expr2));
  def expr2     =  ident ||| number ||| (chr('(') &&& expr &&& chr(')'));
}
