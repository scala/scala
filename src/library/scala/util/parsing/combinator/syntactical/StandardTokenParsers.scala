/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: StdTokenParsers.scala 12242 2007-07-09 09:43:09Z michelou $


package scala.util.parsing.combinator.syntactical

import scala.util.parsing.syntax._
import scala.util.parsing.combinator.lexical.StdLexical

/** This component provides primitive parsers for the standard tokens defined in `StdTokens'.
*
* @author Martin Odersky, Adriaan Moors
 */
class StandardTokenParsers extends StdTokenParsers {
  type Tokens = StdTokens
  val lexical = new StdLexical
}
