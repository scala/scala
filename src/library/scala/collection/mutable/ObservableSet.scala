/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import script._

/** This class is typically used as a mixin. It adds a subscription
 *  mechanism to the `Set` class into which this abstract
 *  class is mixed in. Class `ObservableSet` publishes
 *  events of the type `Message`.
 *
 *  @author  Matthias Zenger
 *  @since   1
 */
@deprecated("observables are deprecated because scripting is deprecated", "2.11.0")
trait ObservableSet[A] extends Set[A] with Publisher[Message[A] with Undoable] {

  type Pub <: ObservableSet[A]

  abstract override def +=(elem: A): this.type = {
    if (!contains(elem)) {
      super.+=(elem)
      publish(new Include(elem) with Undoable { def undo = -=(elem) })
    }
    this
  }

  abstract override def -=(elem: A): this.type = {
    if (contains(elem)) {
      super.-=(elem)
      publish(new Remove(elem) with Undoable { def undo = +=(elem) })
    }
    this
  }

  abstract override def clear(): Unit = {
    super.clear()
    publish(new Reset with Undoable {
      def undo(): Unit = throw new UnsupportedOperationException("cannot undo")
    })
  }
}
