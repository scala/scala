/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable


/** A revertible history is a `History` object which supports
 *  an undo operation. Type variable `Evt` refers to the type
 *  of the published events, `Pub` denotes the publisher type.
 *  Type `Pub` is typically a subtype of `Publisher`.
 *
 *  @tparam Evt   type of the events
 *  @tparam Pub   type of the publisher
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   2.8
 */
class RevertibleHistory[Evt <: Undoable, Pub] extends History[Evt, Pub] with Undoable with Serializable {

  /** Rollback the full history.
   */
  def undo(): Unit = {
    val old = log.toList.reverse
    clear()
    old.foreach { case (sub, event) => event.undo() }
  }
}
