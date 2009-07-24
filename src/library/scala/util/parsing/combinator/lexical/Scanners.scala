/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.parsing.combinator.lexical
import scala.util.parsing.combinator._

import scala.util.parsing.syntax._
import scala.util.parsing.input._

/** <p>
 *    This component provides core functionality for lexical parsers.
 *  </p>
 *  <p>
 *    See its subclasses {@see Lexical} and -- most interestingly
 *    {@see StdLexical}, for more functionality.
 *  </p>
 *
 *  @requires token      a parser that produces a token (from a stream of characters)
 *  @requires whitespace a unit-parser for white-space
 *  @provides Scanner    essentially a parser that parses a stream of characters
 *                       to produce `Token's, which are typically passed to a
 *                       syntactical parser (which operates on `Token's, not on
 *                       individual characters).
 *
 *  @author Martin Odersky, Adriaan Moors
 */
trait Scanners extends Parsers {
  type Elem = Char
  type Token

  /** This token is produced by a scanner {@see Scanner} when scanning failed. */
  def errorToken(msg: String): Token

  /** a parser that produces a token (from a stream of characters) */
  def token: Parser[Token]

  /** a parser for white-space -- its result will be discarded */
  def whitespace: Parser[Any]

  /** <p>
   *    <code>Scanner</code> is essentially(*) a parser that produces `Token's
   *    from a stream of characters. The tokens it produces are typically
   *    passed to parsers in <code>TokenParsers</code>.
   *  </p>
   *  <p>
   *   Note: (*) <code>Scanner</code> is really a `Reader' of `Token's
   *  </p>
   */
  class Scanner(in: Reader[Char]) extends Reader[Token] {
    /** Convenience constructor (makes a character reader out of the given string) */
    def this(in: String) = this(new CharArrayReader(in.toCharArray()))
    private val (tok, rest1, rest2) = whitespace(in) match {
      case Success(_, in1) =>
        token(in1) match {
          case Success(tok, in2) => (tok, in1, in2)
          case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
        }
      case ns: NoSuccess => (errorToken(ns.msg), ns.next, skip(ns.next))
    }
    private def skip(in: Reader[Char]) = if (in.atEnd) in else in.rest

    override def source: java.lang.CharSequence = in.source
    override def offset: Int = in.offset
    def first = tok
    def rest = new Scanner(rest2)
    def pos = rest1.pos
    def atEnd = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })
  }
}

