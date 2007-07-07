/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.combinator.lexical

import scala.util.parsing.syntax._
import scala.util.parsing.input._

/** This component provides core functionality for lexical parsers.
 *<p>
 * See its subclasses {@see Lexical} and -- most interestingly {@see StdLexical},
 * for more functionality.</p>
 *
 * @requires token      a parser that produces a token (from a stream of characters)
 * @requires whitespace a unit-parser for white-space
 * @provides Scanner    essentially a parser that parses a stream of characters to produce `Token's,
 *                      which are typically passed to a syntactical parser (which operates on
 *                      `Token's, not on individual characters)
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait Scanners extends Parsers with Tokens {
  type Elem = char

  /** a parser that produces a token (from a stream of characters) */
  def token: Parser[Token]

  /** a parser for white-space -- its result will be discarded */
  def whitespace: Parser[Any]

  /** `Scanner' is essentially(*) a parser that produces `Token's from a stream of characters.
   * The tokens it produces are typically passed to parsers in `TokenParsers'.
   *
   * Note: (*) `Scanner' is really a `Reader' of `Token's
   */
  class Scanner(in: Reader[char]) extends Reader[Token] {
    /** Convenience constructor (makes a character reader out of the given string) */
    def this(in: String) = this(new CharArrayReader(in.toCharArray()))
    private val Triple(tok, rest1, rest2) = whitespace(in) match {
      case Success(_, in1) =>
        token(in1) match {
          case Success(tok, in2) => Triple(tok, in1, in2)
          case ns: NoSuccess => Triple(errorToken(ns.msg), ns.next, skip(ns.next))
        }
      case ns: NoSuccess => Triple(errorToken(ns.msg), ns.next, skip(ns.next))
    }
    private def skip(in: Reader[char]) = if (in.atEnd) in else in.rest

    def first = tok
    def rest = new Scanner(rest2)
    def pos = rest1.pos
    def atEnd = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })
  }
}

