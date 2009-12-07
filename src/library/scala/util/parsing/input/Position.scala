/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.input

/** <p>
 *    <code>Position</code> is the base trait for objects describing a
 *    position in a ``document''.
 *  </p>
 *  <p>
 *    It provides functionality for:
 *  </p><ul>
 *    <li> generating a visual representation of this position (`longString');
 *    <li> comparing two positions (`<').
 *  </ul>
 *  <p>
 *    To use this class for a concrete kind of ``document'', implement the
 *    <code>lineContents</code> method.
 *  </p>
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait Position {

  /** The line number referred to by the position; line numbers start at 1 */
  def line: Int

  /** The column number referred to by the position; column numbers start at 1 */
  def column: Int

  /** The contents of the line numbered `lnum' (must not contain a new-line character).
   *
   * @param lnum a 1-based integer index into the `document'
   * @return the line at `lnum' (not including a newline)
   */
  protected def lineContents: String

  /** Returns a string representation of the `Position', of the form `line.column' */
  override def toString = ""+line+"."+column

  /** Returns a more ``visual'' representation of this position.
   *  More precisely, the resulting string consists of two lines: <ol>
   *    <li> the line in the document referred to by this position </li>
   *    <li>a caret indicating the column</li></ol>
   *
   *  Example:
   *
   *<pre>    List(this, is, a, line, from, the, document)
   *                  ^</pre>
   */
  def longString = lineContents+"\n"+(" " * (column - 1))+"^"

  /** Compare this position to another, by first comparing their line numbers,
   * and then -- if necessary -- using the columns to break a tie.
   *
   * @param `that' a `Position' to compare to this `Position'
   * @return true if this position's line or (in case of a tie wrt. line numbers)
   *         its column is smaller than the corresponding components of `that'
   */
  def <(that: Position) = {
    this.line < that.line ||
    this.line == that.line && this.column < that.column
  }
}
