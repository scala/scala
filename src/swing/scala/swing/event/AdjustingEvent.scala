/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

/** An event that indicates some editing operation that can be still
 *  in progress.
 *
 *  Example: dragging a slider creates a number of `AdjustmentEvents`
 *  with `adjusting == '''true'''` until the user finally releases the
 *  mouse button.
 */
trait AdjustingEvent extends ComponentEvent {
  def adjusting: Boolean
  def committed: Boolean = !adjusting
}
