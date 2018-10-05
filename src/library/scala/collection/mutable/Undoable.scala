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


/** Classes that mix in the `Undoable` class provide an operation
 *  `undo` which can be used to undo the last operation.
 *
 *  @author  Matthias Zenger
 *  @since   1
 */
trait Undoable {
  /** Undo the last operation.
   */
  def undo(): Unit
}
