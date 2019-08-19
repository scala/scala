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

/** A wrapper for a re-entrant, cached instance of a value of type `T`.
  *
  * Not thread safe.
  */
final class ReusableInstance[T <: AnyRef](make: () => T, enabled: Boolean) {
  private val cached = make()
  private var taken = false

  @inline def using[R](action: T => R): R =
    if (!enabled || taken) action(make())
    else try {
      taken = true
      action(cached)
    } finally taken = false
}

object ReusableInstance {
  def apply[T <: AnyRef](make: => T, enabled: Boolean): ReusableInstance[T] =
    new ReusableInstance[T](make _, enabled)
}