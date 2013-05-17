/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package util.parsing
package combinator
package syntactical

/** This is the core component for token-based parsers.
 *
 *  @author Martin Odersky
 *  @author Adriaan Moors
 */
trait TokenParsers extends Parsers {
  /** `Tokens` is the abstract type of the `Token`s consumed by the parsers in this component. */
  type Tokens <: token.Tokens

  /** `lexical` is the component responsible for consuming some basic kind of
   *  input (usually character-based) and turning it into the tokens
   *  understood by these parsers.
   */
  val lexical: Tokens

  /** The input-type for these parsers*/
  type Elem = lexical.Token

}


