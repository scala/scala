/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util.parsing
package combinator
package token

/** This component provides the notion of `Token`, the unit of information that is passed from lexical
 * parsers in the `Lexical` component to the parsers in the `TokenParsers` component.
 *
 * @author Martin Odersky
 * @author Adriaan Moors
 */
trait Tokens {
  /** Objects of this type are produced by a lexical parser or ``scanner'', and consumed by a parser.
   *
   *  @see [[scala.util.parsing.combinator.syntactical.TokenParsers]]
   */
  abstract class Token {
    def chars: String
  }

  /** A class of error tokens. Error tokens are used to communicate
   *  errors detected during lexical analysis
   */
  case class ErrorToken(msg: String) extends Token {
    def chars = "*** error: "+msg
  }

  /** A class for end-of-file tokens */
  case object EOF extends Token {
    def chars = "<eof>"
  }

  /** This token is produced by a scanner `Scanner` when scanning failed. */
  def errorToken(msg: String): Token = new ErrorToken(msg)
}
