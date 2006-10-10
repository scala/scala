package examples

object parsers1 {

  abstract class Parsers {

    type inputType;

    abstract class Parser {

      type Result = Option[inputType]

      def apply(in: inputType): Result

      /*** p &&& q applies first p, and if that succeeds, then q
       */
      def &&& (q: => Parser) = new Parser {
        def apply(in: inputType): Result = Parser.this.apply(in) match {
          case None => None
          case Some(in1)  => q(in1)
        }
      }

      /*** p ||| q applies first p, and, if that fails, then q.
       */
      def ||| (q: => Parser) = new Parser {
        def apply(in: inputType): Result = Parser.this.apply(in) match {
          case None => q(in)
          case s => s
        }
      }
    }

    val empty = new Parser {
      def apply(in: inputType): Result = Some(in)
    }

    val fail = new Parser {
      def apply(in: inputType): Result = None
    }

    def opt(p: Parser): Parser = p ||| empty    // p? = (p | <empty>)
    def rep(p: Parser): Parser = opt(rep1(p))   // p* = [p+]
    def rep1(p: Parser): Parser = p &&& rep(p)  // p+ = p p*
  }

  trait ListParsers extends Parsers {
    def chr(p: char => boolean): Parser
    def chr(c: char): Parser = chr((d: char) => d == c)

    def letter    : Parser = chr((c: char) => Character.isLetter(c))
    def digit     : Parser = chr((c: char) => Character.isDigit(c))

    def ident     : Parser = letter &&& rep(letter ||| digit)
    def number    : Parser = digit &&& rep(digit)
    def list      : Parser = chr('(') &&& listElems &&& chr(')')
    def listElems : Parser = expr &&& (chr(',') &&& listElems ||| empty)
    def expr      : Parser = ident ||| number ||| list
  }

  trait ExprParsers extends Parsers {
    def chr(p: char => boolean): Parser
    def chr(c: char): Parser = chr((d: char) => d == c)

    def digit     : Parser = chr((c: char) => Character.isDigit(c))
    def number    : Parser = digit &&& rep(digit)
    def summand   : Parser = number ||| chr('(') &&& expr &&& chr(')')
    def expr      : Parser = summand &&& rep(chr('+') &&& summand)
  }

  class ParseString(s: String) extends Parsers {
    type inputType = Int
    val input = 0
    def chr(p: char => boolean) = new Parser {
      def apply(in: int): Parser#Result =
        if (in < s.length() && p(s charAt in)) Some(in + 1)
        else None
    }
  }

  object TestList {
    def main(args: Array[String]): Unit =
      Console.println(
        if (args.length == 1) {
          val ps = new ParseString(args(0)) with ListParsers
          ps.expr(ps.input) match {
            case Some(n) =>
              "parsed: " + args(0).substring(0, n)
            case None =>
              "nothing parsed"
          }
        }
        else
          "usage: java examples.TestList <expr-string>"
      )
  }

  object TestExpr {
    def main(args: Array[String]): unit =
      Console.println(
        if (args.length == 1) {
          val ps = new ParseString(args(0)) with ExprParsers
          ps.expr(ps.input) match {
            case Some(n) =>
              "parsed: " + args(0).substring(0, n)
            case None =>
              "nothing parsed"
          }
        }
        else
          "usage: java examples.TestExpr <expr-string>"
      )
  }

  def main(args: Array[String]): Unit = {
    TestList.main(Array("(a,b,(1,2))"))
    TestExpr.main(Array("2+3+(4+1)"))
  }

}
