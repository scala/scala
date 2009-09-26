/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import script._


/** This class is typically used as a mixin. It adds a subscription
 *  mechanism to the <code>Map</code> class into which this abstract
 *  class is mixed in. Class <code>ObservableMap</code> publishes
 *  events of the type <code>Message</code>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 *  @since   1
 */
trait ObservableMap[A, B, This <: ObservableMap[A, B, This]]
      extends Map[A, B]
      with Publisher[Message[(A, B)] with Undoable, This]
{ self: This =>

  abstract override def += (kv: (A, B)): this.type = {
    val (key, value) = kv

    get(key) match {
      case None =>
        super.+=(kv)
        publish(new Include((key, value)) with Undoable {
          def undo = -=(key)
        })
      case Some(old) =>
        super.+=(kv)
        publish(new Update((key, value)) with Undoable {
          def undo = +=((key, old))
        })
    }
    this
  }

  abstract override def -= (key: A): this.type = {
    get(key) match {
      case None =>
      case Some(old) =>
        super.-=(key)
        publish(new Remove((key, old)) with Undoable {
          def undo = update(key, old)
        })
    }
    this
  }

  abstract override def clear(): Unit = {
    super.clear
    publish(new Reset with Undoable {
      def undo: Unit = throw new UnsupportedOperationException("cannot undo")
    })
  }
}
