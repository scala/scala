/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
  * An instance is created eagerly, then more instances
  * are allocated as needed on re-entry. Once allocated,
  * cached instances are not reclaimed for the life of this ReusableInstance.
  *
  * Not thread safe.
  */
final class ReusableInstance[T <: AnyRef] private (make: => T, initialSize: Int) {
  private[this] val cache = if (initialSize > 0) new ArrayBuffer[T](initialSize).tap(_.addOne(make)) else null
  private[this] var taken = 0

  @inline def using[R](action: T => R): R =
    if (cache == null)
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

  def apply[T <: AnyRef](make: => T, initialSize: Int): ReusableInstance[T] = new ReusableInstance[T](make, initialSize)

  def apply[T <: AnyRef](make: => T): ReusableInstance[T] =
    apply(make, InitialSize)
  def apply[T <: AnyRef](make: => T, enabled: Boolean): ReusableInstance[T] =
    if (enabled) apply(make) else apply(make, -1)
  def apply[T <: AnyRef](make: => T, initialSize: Int, enabled: Boolean): ReusableInstance[T] =
    if (enabled) apply(make, initialSize) else apply(make, -1)
}
