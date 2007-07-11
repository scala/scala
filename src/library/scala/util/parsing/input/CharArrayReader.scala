/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.util.parsing.input

/** An object encapsulating basic character constants
 *
 * @author Martin Odersky, Adriaan Moors
 */
object CharArrayReader {
  final val EofCh = '\032'
  final val CR = '\015'
}

/** A character array reader reads a stream of characters (keeping track of their positions)
 * from an array.
 *
 * @param source an array of characters
 * @param index  starting offset into the array; the first element returned will be `source(index)'
 * @param line   the line number of the first element (counting from index `0' of `source')
 * @param column the column number of the first element (counting from index `0' of `source')
 *
 * @author Martin Odersky, Adriaan Moors
 */
class CharArrayReader(source: Array[Char], index: Int, line: Int, column: Int) extends Reader[Char] {
  import CharArrayReader._

  /** Construct a <code>CharArrayReader</code> with its first element at
   *  <code>source(0)</code> and position <code>(1,1)</code>.
   */
  def this(source: Array[Char]) = this(source, 0, 1, 1)

  private var i = index
  if (i + 1 < source.length && source(i) == CR && source(i + 1) == '\n') i += 1

  // see `first' in `Reader'
  def first = if (i == source.length) EofCh else source(i)

  // see `rest' in `Reader'
  def rest: CharArrayReader = {
    val ch = first
    if (ch == EofCh) this
    else if (ch == '\n') new CharArrayReader(source, i + 1, line + 1, 1)
    else new CharArrayReader(source, i + 1, line, column + 1)
  }

  // see `pos' in `Reader'
  def pos: Position = new CharArrayPosition(source, line, column)

  // see `atEnd' in `Reader'
  def atEnd = i == source.length
}
