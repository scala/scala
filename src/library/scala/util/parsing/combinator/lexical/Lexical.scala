/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.combinator.lexical

import scala.util.parsing.syntax._
import scala.util.parsing.input.CharArrayReader.EofCh

/** This component complements the `Scanners' component with common operations for lexical parsers.
 *<p>
 * {@see StdLexical} for a concrete implementation for a simple, Scala-like language.</p>
 *
 * @author Martin Odersky, Adriaan Moors
 */
abstract class Lexical extends Scanners with Tokens {

  /** A character-parser that matches a letter (and returns it)*/
  def letter = elem("letter", _.isLetter)

  /** A character-parser that matches a digit (and returns it)*/
  def digit = elem("digit", _.isDigit)

  /** A character-parser that matches any character except the ones given in `cs' (and returns it)*/
  def chrExcept(cs: char*) = elem("", ch => (cs forall (ch !=)))

  /** A character-parser that matches a white-space character (and returns it)*/
  def whitespaceChar = elem("space char", ch => ch <= ' ' && ch != EofCh)
}