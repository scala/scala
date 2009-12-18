/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package parsing

import scala.io.Source
import scala.xml.dtd._
import Utility.Escapes.{ pairs => unescape }

/** This is not a public trait - it contains common code shared
 *  between the library level XML parser and the compiler's.
 *  All members should be accessed through those.
 */
private[scala] trait MarkupParserCommon extends TokenTests {
  // type InputType    // Source, CharArrayReader
  // type HandleType   // MarkupHandler, SymbolicXMLBuilder
  // type PositionType // Int, Position

  def ch: Char
  def nextch: Char
  def xHandleError(that: Char, msg: String): Unit
  def reportSyntaxError(str: String): Unit
  def reportSyntaxError(pos: Int, str: String): Unit
  def eof: Boolean

  def xToken(that: Char) {
    if (ch == that) nextch
    else xHandleError(that, "'%s' expected instead of '%s'".format(that, ch))
  }
  def xToken(that: Seq[Char]) { that foreach xToken }

  /** scan [S] '=' [S]*/
  def xEQ = { xSpaceOpt; xToken('='); xSpaceOpt }

  /** skip optional space S? */
  def xSpaceOpt = while (isSpace(ch) && !eof) nextch

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  def xSpace =
    if (isSpace(ch)) { nextch; xSpaceOpt }
    else xHandleError(ch, "whitespace expected")

  //
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }
}
