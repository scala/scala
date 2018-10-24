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

package scala.concurrent

/** This class ...
 *
 *  @author  Martin Odersky
 */
@deprecated("use java.util.concurrent.locks.Lock", "2.11.2")
class Lock {
  var available = true

  def acquire() = synchronized {
    while (!available) wait()
    available = false
  }

  def release() = synchronized {
    available = true
    notify()
  }
}
