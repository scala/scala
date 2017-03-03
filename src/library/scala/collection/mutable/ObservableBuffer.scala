/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import script._

/** This class is typically used as a mixin. It adds a subscription
 *  mechanism to the `Buffer` class into which this abstract
 *  class is mixed in. Class `ObservableBuffer` publishes
 *  events of the type `Message`.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 */
@deprecated("observables are deprecated because scripting is deprecated", "2.11.0")
trait ObservableBuffer[A] extends Buffer[A] with Publisher[Message[A] with Undoable] {
  type Pub <: ObservableBuffer[A]

  abstract override def +=(element: A): this.type = {
    super.+=(element)
    publish(new Include(End, element) with Undoable {
      def undo() { trimEnd(1) }
    })
    this
  }

  abstract override def ++=(xs: TraversableOnce[A]): this.type = {
    for (x <- xs) this += x
    this
  }

  abstract override def +=:(element: A): this.type = {
    super.+=:(element)
    publish(new Include(Start, element) with Undoable {
      def undo() { trimStart(1) }
    })
    this
  }

  abstract override def update(n: Int, newelement: A): Unit = {
    val oldelement = apply(n)
    super.update(n, newelement)
    publish(new Update(Index(n), newelement) with Undoable {
      def undo() { update(n, oldelement) }
    })
  }

  abstract override def remove(n: Int): A = {
    val oldelement = apply(n)
    super.remove(n)
    publish(new Remove(Index(n), oldelement) with Undoable {
      def undo() { insert(n, oldelement) }
    })
    oldelement
  }

  abstract override def clear(): Unit = {
    super.clear()
    publish(new Reset with Undoable {
      def undo() { throw new UnsupportedOperationException("cannot undo") }
    })
  }

  abstract override def insertAll(n: Int, elems: scala.collection.Traversable[A]) {
    super.insertAll(n, elems)
    var curr = n - 1
    val msg = elems.foldLeft(new Script[A]() with Undoable {
      def undo() { throw new UnsupportedOperationException("cannot undo") }
    }) {
      case (msg, elem) =>
        curr += 1
        msg += Include(Index(curr), elem)
    }
    publish(msg)
  }

}
