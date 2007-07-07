/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.input

/** `CharArrayPosition' implements the general `Position' class for
 * documents represented by an `Array' of `char's.
 *
 *  @param source The contents of the document in which this position is contained
 *  @param line   The line number of the position (1-based)
 *  @param columm The column number of the position (1-based)
 *
 * @author Martin Odersky, Adriaan Moors
 */
class CharArrayPosition(val source: Array[char], val line: int, val column: int) extends Position {

  // TODO: this could be implemented more high-level:
  // return the string representation of the sub-array of source that starts after the
  // (lnum-1)'ed '\n' up to (but not including) the (lnum)'ed '\n'
  protected def lineContents(lnum: int) = {
    var i = 0
    var l = 1
    while (i < source.length && l < lnum) {
      while (i < source.length && source(i) != '\n') i = i + 1
      i = i + 1
      l = l + 1
    }
    var chars = new StringBuffer
    while (i < source.length && source(i) != '\n') {
      chars append source(i)
      i = i + 1
    }
    chars.toString
  }
}

