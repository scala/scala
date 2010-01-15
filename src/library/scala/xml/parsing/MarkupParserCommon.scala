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
  private final val SU: Char = 0x1A
  protected def unreachable = Predef.error("Cannot be reached.")

  // type HandleType   // MarkupHandler, SymbolicXMLBuilder

  type InputType        // Source, CharArrayReader
  type PositionType     // Int, Position

  /** Create a lookahead reader which does not influence the input */
  def lookahead(): BufferedIterator[Char]

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

  /** Take characters from input stream until given String "until"
   *  is seen.  Once seen, the accumulated characters are passed
   *  along with the current Position to the supplied handler function.
   */
  protected def xTakeUntil[T](
    handler: (PositionType, String) => T,
    positioner: () => PositionType,
    until: String): T =
  {
    val sb = new StringBuilder
    val head = until charAt 0
    val rest = until drop 1

    while (true) {
      if (ch == head && peek(rest))
        return handler(positioner(), sb.toString)
      else if (ch == SU)
        xHandleError(ch, "")  // throws TruncatedXML in compiler

      sb append ch
      nextch
    }
    unreachable
  }

  /** Create a non-destructive lookahead reader and see if the head
   *  of the input would match the given String.  If yes, return true
   *  and drop the entire String from input; if no, return false
   *  and leave input unchanged.
   */
  private def peek(lookingFor: String): Boolean =
    (lookahead() take lookingFor.length sameElements lookingFor.iterator) && {
      // drop the chars from the real reader (all lookahead + orig)
      (0 to lookingFor.length) foreach (_ => nextch)
      true
    }
}
