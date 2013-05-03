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

/** This component provides the standard `Token`s for a simple, Scala-like language.
 *
 * @author Martin Odersky
 * @author Adriaan Moors
 */
trait StdTokens extends Tokens {
  /** The class of keyword tokens */
  case class Keyword(chars: String) extends Token {
    override def toString = "`"+chars+"'"
  }

  /** The class of numeric literal tokens */
  case class NumericLit(chars: String) extends Token {
    override def toString = chars
  }

  /** The class of string literal tokens */
  case class StringLit(chars: String) extends Token {
    override def toString = "\""+chars+"\""
  }

  /** The class of identifier tokens */
  case class Identifier(chars: String) extends Token {
    override def toString = "identifier "+chars
  }
}
