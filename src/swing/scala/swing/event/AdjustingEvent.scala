/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

/** <p>
 *    An event that indicates some editing operation that can be still in
 *    progress.<br/>
 *    Example: dragging a slider creates a number of <code>AdjustmentEvents</code>
 *    with <code>adjusting == true</code> until the user finally releases the
 *    mouse button.
 *  </p>
 */
trait AdjustingEvent extends ComponentEvent {
  def adjusting: Boolean
  def committed: Boolean = !adjusting
}
