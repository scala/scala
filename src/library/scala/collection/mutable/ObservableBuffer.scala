/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.script._

/** This class is typically used as a mixin. It adds a subscription
 *  mechanism to the <code>Buffer</code> class into which this abstract
 *  class is mixed in. Class <code>ObservableBuffer</code> publishes
 *  events of the type <code>Message</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait ObservableBuffer[A, This <: ObservableBuffer[A, This]]
      extends Buffer[A]
      with Publisher[Message[A] with Undoable, This]
{ self: This =>

  abstract override def +=(element: A): this.type = {
    super.+=(element)
    publish(new Include(End, element) with Undoable {
      def undo() { trimEnd(1) }
    })
    this
  }

  abstract override def +:(element: A): this.type = {
    super.+:(element)
    publish(new Include(Start, element) with Undoable {
      def undo() { trimStart(1) }
    })
    this
  }

  abstract override def update(n: Int, newelement: A): Unit = {
    val oldelement = apply(n)
    super.update(n, newelement)
    publish(new Update(Index(n), newelement) with Undoable {
      def undo { update(n, oldelement) }
    })
  }

  abstract override def remove(n: Int): A = {
    val oldelement = apply(n)
    super.remove(n)
    publish(new Remove(Index(n), oldelement) with Undoable {
      def undo { insert(n, oldelement) }
    })
    oldelement
  }

  abstract override def clear(): Unit = {
    super.clear
    publish(new Reset with Undoable {
      def undo { throw new UnsupportedOperationException("cannot undo") }
    })
  }
}
