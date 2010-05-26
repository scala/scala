/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.io

/** <p>
 *    The object <code>Position</code> provides convenience methods to encode
 *    line and column number in one single integer. The encode line (column)
 *    numbers range from 0 to <code>LINE_MASK</code>
 *    (<code>COLUMN_MASK</code>), where 0 indicates that the line (column) is
 *    the undefined and 1 represents the first line (column). Line (Column)
 *    numbers greater than <code>LINE_MASK</code>
 *    (<code>COLUMN_MASK</code>) are replaced by <code>LINE_MASK</code>
 *    (<code>COLUMN_MASK</code>). Furthermore, if the encoded line number is
 *    <code>LINE_MASK</code>, the column number is always set to 0.
 *  </p>
 *  <p>
 *    The following properties hold:
 *  </p>
 *  <ul>
 *    <li>
 *      the undefined position is 0: <code>encode(0,0) == 0</code>
 *    </li>
 *    <li>
 *      encodings are non-negative : <code>encode(line,column) >= 0</code>
 *    </li>
 *    <li>
 *      position order is preserved:
 *      <code>(line1 &lt; line2) || (line1 == line2 &amp;&amp; column1 &lt; column2)</code>
 *      <div>implies</div>
 *      <code>encode(line1,column1) &lt;= encode(line2,column2)</code>
 *    </li>
 *  </ul>
 *
 *  @author Burak Emir (translated from work by Matthias Zenger and others)
 */
object Position {

  /** Number of bits used to encode the line number */
  final val LINE_BITS   = 20
  /** Number of bits used to encode the column number */
  final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31

  /** Mask to decode the line number */
  final val LINE_MASK   = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  final val COLUMN_MASK = (1 << COLUMN_BITS) - 1

  /** The undefined position */
  final val NOPOS       = 0

  /** The first position in a source file */
  final val FIRSTPOS    = encode(1, 1)

  /** Encodes a position into a single integer. */
  final def encode(line: Int, column: Int): Int = {
    var line1, column1 = 0
    if (line < 0)
      throw new IllegalArgumentException(line + " < 0")
    if ((line == 0) && (column != 0))
      throw new IllegalArgumentException(line + "," + column + " not allowed")
    if (column < 0)
      throw new IllegalArgumentException(line + "," + column + " not allowed")

    if (line >= LINE_MASK) {
      line1 = LINE_MASK
      column1 = 0
    }
    else {
      line1 = line
      if (column > COLUMN_MASK)
        column1 = COLUMN_MASK
      else
        column1 = column
    }
    (line1 << COLUMN_BITS) | column1
  }

  /** Returns the line number of the encoded position. */
  final def line(pos: Int): Int =
    (pos >> COLUMN_BITS) & LINE_MASK

  /** Returns the column number of the encoded position. */
  final def column(pos: Int): Int =
    pos & COLUMN_MASK

  /** Returns a string representation of the encoded position. */
  def toString(pos: Int): String = {
    val sb = new StringBuilder
    sb append line(pos)
    sb append ':'
    sb append column(pos)
    sb.toString()
  }
}
