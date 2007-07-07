/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.parsing.input

/** `Position' is the base class for objects describing a position in a ``document''.
 *<p>
 * It provides functionality for: <ul>
 *  <li> generating a visual representation of this position (`longString');
 *  <li> comparing two positions (`<').
 * </ul></p>
 *<p>
 * To use this class for a concrete kind of ``document'', implement the `lineContents' method.</p>
 *
 * @author Martin Odersky, Adriaan Moors
 */
trait Position {

  /** The line number referred to by the position; line numbers start at 1 */
  def line: int

  /** The column number referred to by the position; column numbers start at 1 */
  def column: int

  /** The contents of the line numbered `lnum' (must not contain a new-line character).
   *
   * @param lnum a 1-based integer index into the `document'
   * @return the line at `lnum' (not including a newline)
   */
  protected def lineContents(lnum: int): String

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
  def longString = lineContents(line)+"\n"+List.toString(List.make(column-1, ' '))+"^"
/*                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  TODO: it would be nicer to be able to write       (' '*column-1)
    --> add * method to scala.runtime.RichChar?
 class RichChar { ...
   /** Returns a string that consists of `n' occurrences of this character. */
   def *(n: int) = {
     val chars = new StringBuffer
     for (val i <- 1 until n) chars append this
     chars.toString
   }
 }*/

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
