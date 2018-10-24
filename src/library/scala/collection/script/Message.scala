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

import mutable.ArrayBuffer

/** Class `Message` represents messages that are issued by observable
 *  collection classes whenever a data structure is changed. Class `Message`
 *  has several subclasses for the various kinds of events: `Update`
 *  `Remove`, `Include`, `Reset`, and `Script`.
 *
 *  @author  Matthias Zenger
 *  @since   2.8
 */
@deprecated("scripting is deprecated", "2.11.0")
trait Message[+A]

/** This observable update refers to inclusion operations that add new elements
 *  to collection classes.
 *
 *  @author  Matthias Zenger
 */
@deprecated("scripting is deprecated", "2.11.0")
case class Include[+A](location: Location, elem: A) extends Message[A] {
  def this(elem: A) = this(NoLo, elem)
}

/** This observable update refers to destructive modification operations
 *  of elements from collection classes.
 *
 *  @author  Matthias Zenger
 */
@deprecated("scripting is deprecated", "2.11.0")
case class Update[+A](location: Location, elem: A) extends Message[A] {
  def this(elem: A) = this(NoLo, elem)
}

/** This observable update refers to removal operations of elements
 *  from collection classes.
 *
 *  @author  Matthias Zenger
 */
@deprecated("scripting is deprecated", "2.11.0")
case class Remove[+A](location: Location, elem: A) extends Message[A] {
  def this(elem: A) = this(NoLo, elem)
}

/** This command refers to reset operations.
 *
 *  @author  Matthias Zenger
 */
@deprecated("scripting is deprecated", "2.11.0")
case class Reset[+A]() extends Message[A]

/** Objects of this class represent compound messages consisting
 *  of a sequence of other messages.
 *
 *  @author  Matthias Zenger
 */
@deprecated("scripting is deprecated", "2.11.0")
class Script[A] extends ArrayBuffer[Message[A]] with Message[A] {

  override def toString(): String = {
    var res = "Script("
    val it = this.iterator
    var i = 1
    while (it.hasNext) {
      if (i > 1)
        res = res + ", "
      res = res + "[" + i + "] " + it.next
      i += 1
    }
    res + ")"
  }
}
