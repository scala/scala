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

package scala.reflect.internal.util

import scala.collection.mutable.ArrayBuffer
import scala.util.chaining._

/** A wrapper for a list of cached instances of a type `T`.
  * The wrapper is recursion-reentrant: several instances are kept, so
  * at each depth of reentrance we are reusing the instance for that.
  *
  * An instance is created upon creating this object, and more instances
  * are allocated dynamically, on demand, when reentrance occurs.
  *
  * Not thread safe.
  */
final class ReusableInstance[T <: AnyRef] private (make: => T, enabled: Boolean) {
  private[this] val cache = if (enabled) new ArrayBuffer[T](ReusableInstance.InitialSize).tap(_.addOne(make)) else null
  private[this] var taken = 0

  @inline def using[R](action: T => R): R =
    if (!enabled)
      action(make)
    else {
      if (taken == cache.size)
        cache += make
      taken += 1
      try action(cache(taken-1)) finally taken -= 1
    }
}

object ReusableInstance {
  private final val InitialSize = 4

  def apply[T <: AnyRef](make: => T): ReusableInstance[T]                   = new ReusableInstance[T](make, enabled = true)
  def apply[T <: AnyRef](make: => T, enabled: Boolean): ReusableInstance[T] = new ReusableInstance[T](make, enabled = enabled)
}
