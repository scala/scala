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


/** A revertible history is a `History` object which supports
 *  an undo operation. Type variable `Evt` refers to the type
 *  of the published events, `Pub` denotes the publisher type.
 *  Type `Pub` is typically a subtype of `Publisher`.
 *
 *  @tparam Evt   type of the events
 *  @tparam Pub   type of the publisher
 *
 *  @author  Matthias Zenger
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
