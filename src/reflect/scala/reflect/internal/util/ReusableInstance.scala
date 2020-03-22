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
package reflect
package internal
package util

import scala.collection.mutable.ArrayBuffer

/** A wrapper for a list of cached instances of a value of type `T`.
  * The wrapper is recursion-reentrant: several instances are kept, so
  * at each depth of reentrance we are reusing the instance for that.
  *
  * An instance is created upon creating this object, and more instances
  * are allocated dynamically, on demand, when reentrance occurs.
  *
  * Not thread safe.
  */
final class ReusableInstance[T <: AnyRef](make: () => T, enabled: Boolean) {
  private[this] val cached = new ArrayBuffer[T]
  cached += make()

  private var taken: Int = 0

  @inline def using[R](action: T => R): R =
    if (!enabled)
      action(make())
    else {
      if (taken == cached.size)
        cached += make()
      try {
        taken += 1
        action(cached(taken-1))
      } finally taken -= 1
    }
}

object ReusableInstance {
  def apply[T <: AnyRef](make: => T, enabled: Boolean): ReusableInstance[T] =
    new ReusableInstance[T](make _, enabled)
}