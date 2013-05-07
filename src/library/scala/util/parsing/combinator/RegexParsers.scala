/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package util.parsing.combinator

import java.util.regex.Pattern
import scala.util.matching.Regex
import scala.util.parsing.input._
import scala.collection.immutable.PagedSeq
import scala.language.implicitConversions

/** The ''most important'' differences between `RegexParsers` and
 *  [[scala.util.parsing.combinator.Parsers]] are:
 *
 *  - `Elem` is defined to be [[scala.Char]]
 *  - There's an implicit conversion from [[java.lang.String]] to `Parser[String]`,
 *    so that string literals can be used as parser combinators.
 *  - There's an implicit conversion from [[scala.util.matching.Regex]] to `Parser[String]`,
 *    so that regex expressions can be used as parser combinators.
 *  - The parsing methods call the method `skipWhitespace` (defaults to `true`) and, if true,
 *    skip any whitespace before each parser is called.
 *  - Protected val `whiteSpace` returns a regex that identifies whitespace.
 *
 *  For example, this creates a very simple calculator receiving `String` input:
 *
 *  {{{
 *  object Calculator extends RegexParsers {
 *    def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
 *    def factor: Parser[Double] = number | "(" ~> expr <~ ")"
 *    def term  : Parser[Double] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
 *      case number ~ list => (number /: list) {
 *        case (x, "*" ~ y) => x * y
 *        case (x, "/" ~ y) => x / y
 *      }
 *    }
 *    def expr  : Parser[Double] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
 *      case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
 *        case (x, "+" ~ y) => x + y
 *        case (x, "-" ~ y) => x - y
 *      }
 *    }
 *
 *    def apply(input: String): Double = parseAll(expr, input) match {
 *      case Success(result, _) => result
 *      case failure : NoSuccess => scala.sys.error(failure.msg)
 *    }
 *  }
 *  }}}
 */
trait RegexParsers extends Parsers {

  type Elem = Char

  protected val whiteSpace = """\s+""".r

  def skipWhitespace = whiteSpace.toString.length > 0

  /** Method called to handle whitespace before parsers.
   *
   *  It checks `skipWhitespace` and, if true, skips anything
   *  matching `whiteSpace` starting from the current offset.
   *
   *  @param source  The input being parsed.
   *  @param offset  The offset into `source` from which to match.
   *  @return        The offset to be used for the next parser.
   */
  protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhitespace)
      (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => offset + matched.end
        case None => offset
      }
    else
      offset

  /** A parser that matches a literal string */
  implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      else  {
        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  /** A parser that matches a regex string */
  implicit def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(source.subSequence(start, start + matched.end).toString,
                  in.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  /** `positioned` decorates a parser's result with the start position of the input it consumed.
   * If whitespace is being skipped, then it is skipped before the start position is recorded.
   *
   * @param p a `Parser` whose result conforms to `Positional`.
   * @return A parser that has the same behaviour as `p`, but which marks its result with the
   *         start position of the input it consumed after whitespace has been skipped, if it
   *         didn't already have a position.
   */
  override def positioned[T <: Positional](p: => Parser[T]): Parser[T] = {
    val pp = super.positioned(p)
    new Parser[T] {
      def apply(in: Input) = {
        val offset = in.offset
        val start = handleWhiteSpace(in.source, offset)
        pp(in.drop (start - offset))
      }
    }
  }

  override def phrase[T](p: Parser[T]): Parser[T] =
    super.phrase(p <~ opt("""\z""".r))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parse[T](p: Parser[T], in: Reader[Char]): ParseResult[T] =
    p(in)

  /** Parse some prefix of character sequence `in` with parser `p`. */
  def parse[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    p(new CharSequenceReader(in))

  /** Parse some prefix of reader `in` with parser `p`. */
  def parse[T](p: Parser[T], in: java.io.Reader): ParseResult[T] =
    p(new PagedSeqReader(PagedSeq.fromReader(in)))

  /** Parse all of reader `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: Reader[Char]): ParseResult[T] =
    parse(phrase(p), in)

  /** Parse all of reader `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: java.io.Reader): ParseResult[T] =
    parse(phrase(p), in)

  /** Parse all of character sequence `in` with parser `p`. */
  def parseAll[T](p: Parser[T], in: java.lang.CharSequence): ParseResult[T] =
    parse(phrase(p), in)
}
