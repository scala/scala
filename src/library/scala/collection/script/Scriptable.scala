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
package script

/** Classes that mix in the `Scriptable` class allow messages to be sent to
 *  objects of that class.
 *
 *  @author  Matthias Zenger
 *  @since   2.8
 */
@deprecated("scripting is deprecated", "2.11.0")
trait Scriptable[A] {
  /** Send a message to this scriptable object.
   */
  def <<(cmd: Message[A]): Unit
}
