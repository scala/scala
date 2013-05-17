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

import token._
import lexical.StdLexical
import scala.language.implicitConversions

/** This component provides primitive parsers for the standard tokens defined in `StdTokens`.
*
* @author Martin Odersky, Adriaan Moors
 */
class StandardTokenParsers extends StdTokenParsers {
  type Tokens = StdTokens
  val lexical = new StdLexical

  //an implicit keyword function that gives a warning when a given word is not in the reserved/delimiters list
  override implicit def keyword(chars : String): Parser[String] =
    if(lexical.reserved.contains(chars) || lexical.delimiters.contains(chars)) super.keyword(chars)
    else failure("You are trying to parse \""+chars+"\", but it is neither contained in the delimiters list, nor in the reserved keyword list of your lexical object")

}
