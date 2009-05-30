/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.parsing.input

/** <code>CharArrayPosition</code> implements the general <code>Position</code>
 *  class for documents represented by an <code>Array</code> of `char's.
 *
 *  @param source The contents of the document in which this position is contained
 *  @param line   The line number of the position (1-based)
 *  @param columm The column number of the position (1-based)
 *
 * @author Martin Odersky, Adriaan Moors
 */
@deprecated("use OffsetPosition instead")
class CharArrayPosition(val source: Array[Char], val line: Int, val column: Int) extends Position {

  // TODO: this could be implemented more high-level:
  // return the string representation of the sub-array of source that starts
  // after the (lnum-1)'ed '\n' up to (but not including) the (lnum)'ed '\n'
  protected def lineContents = {
    var i = 0
    var l = 1
    while (i < source.length && l < line) {
      while (i < source.length && source(i) != '\n') i += 1
      i += 1
      l += 1
    }
    var chars = new StringBuffer
    while (i < source.length && source(i) != '\n') {
      chars append source(i)
      i += 1
    }
    chars.toString
  }
}

