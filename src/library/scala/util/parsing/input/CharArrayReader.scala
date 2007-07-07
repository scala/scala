/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
class CharArrayReader(source: Array[char], index: int, line: int, column: int) extends Reader[char] {
  import CharArrayReader._

  /** Construct a `CharArrayReader' with its first element at `source(0)' and position `(1,1)'
   */
  def this(source: Array[char]) = this(source, 0, 1, 1)

  private var i = index
  if (i + 1 < source.length && source(i) == CR && source(i + 1) == '\n') i = i + 1

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
