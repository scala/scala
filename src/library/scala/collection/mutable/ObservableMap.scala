/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This class is typically used as a mixin. It adds a subscription
 *  mechanism to the <code>Map</code> class into which this abstract
 *  class is mixed in. Class <code>ObservableMap</code> publishes
 *  events of the type <code>Message</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait ObservableMap[A, B, This <: ObservableMap[A, B, This]] requires This
      extends Map[A, B]
      with Publisher[Message[Pair[A, B]]
      with Undoable, This]
{

  abstract override def update(key: A, value: B): Unit = get(key) match {
    case None =>
      super.update(key, value)
      publish(new Include(Pair(key, value)) with Undoable {
        def undo = -=(key)
      })
    case Some(old) =>
      super.update(key, value)
      publish(new Update(Pair(key, value)) with Undoable {
        def undo = update(key, old)
      })
  }

  abstract override def -=(key: A): Unit = get(key) match {
    case None =>
    case Some(old) =>
      super.-=(key)
      publish(new Remove(Pair(key, old)) with Undoable {
        def undo = update(key, old)
      })
  }

  abstract override def clear: Unit = {
    super.clear
    publish(new Reset with Undoable { def undo: Unit = error("cannot undo") })
  }
}
