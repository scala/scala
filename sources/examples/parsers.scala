package examples;

object parsers {

abstract class Parsers {

  type intype;

  abstract class Parser {

    type Result = Option[intype];

    def apply(in: intype): Result;

    /*** p &&& q applies first p, and if that succeeds, then q
     */
    def &&& (def q: Parser) = new Parser {
      def apply(in: intype): Result = Parser.this.apply(in) match {
        case None => None
        case Some(in1)  => q(in1)
      }
    }

    /*** p ||| q applies first p, and, if that fails, then q.
     */
    def ||| (def q: Parser) = new Parser {
      def apply(in: intype): Result = Parser.this.apply(in) match {
        case None => q(in)
        case s => s
      }
    }
  }

  val empty = new Parser {
    def apply(in: intype): Result = Some(in)
  }

  val fail = new Parser {
    def apply(in: intype): Result = None
  }

  def opt(p: Parser): Parser = p ||| empty;    // p? = (p | <empty>)
  def rep(p: Parser): Parser = opt(rep1(p));   // p* = [p+]
  def rep1(p: Parser): Parser = p &&& rep(p);  // p+ = p p*
}

  abstract class ListParsers extends Parsers {
    def chr(p: char => boolean): Parser;
    def chr(c: char): Parser = chr(d: char => d == c);

    def letter    : Parser = chr(Character.isLetter);
    def digit     : Parser = chr(Character.isDigit);

    def ident     : Parser = letter &&& rep(letter ||| digit);
    def number    : Parser = digit &&& rep(digit);
    def list      : Parser = chr('(') &&& listElems &&& chr(')');
    def listElems : Parser = expr &&& (chr(',') &&& listElems ||| empty);
    def expr      : Parser = ident ||| number ||| list;
  }

  abstract class ExprParsers extends Parsers {
    def chr(p: char => boolean): Parser;
    def chr(c: char): Parser = chr(d: char => d == c);

    def digit     : Parser = chr(Character.isDigit);
    def number    : Parser = digit &&& rep(digit);
    def summand   : Parser = number ||| chr('(') &&& expr &&& chr(')');
    def expr      : Parser = summand &&& rep(chr('+') &&& summand)
  }

  class ParseString(s: String) extends Parsers {
    type intype = int;
    val input = 0;
    def chr(p: char => boolean) = new Parser {
      def apply(in: int): Parser#Result =
        if (in < s.length() && p(s charAt in)) Some(in + 1);
        else None;
    }
  }

  object TestList {

    def main(args: Array[String]): unit =
      if (args.length == 1) {
        val ps = new ListParsers with ParseString(args(0));
        ps.expr(ps.input) match {
          case Some(n) =>
            Console.println("parsed: " + args(0).substring(0, n));
          case None =>
            Console.println("nothing parsed");
        }
      }
      else
        Console.println("usage: java examples.TestList <expr-string>");
  }

  object TestExpr {

    def main(args: Array[String]): unit =
      if (args.length == 1) {
        val ps = new ExprParsers with ParseString(args(0));
        ps.expr(ps.input) match {
          case Some(n) =>
            Console.println("parsed: " + args(0).substring(0, n));
          case None =>
            Console.println("nothing parsed");
        }
      }
      else
        Console.println("usage: java examples.TestExpr <expr-string>");
  }

  def main(args: Array[String]): Unit = {
    TestList.main(Array("(a,b,(1,2))"));
    TestExpr.main(Array("2+3+(4+1)"))
  }

}
