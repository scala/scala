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

package scala

/** An interface containing operations for equality.
 *  The only method not already present in class `AnyRef` is `canEqual`.
 */
trait Equals extends Any {
  /** Checks whether this instance can possibly equal `that`.
   *
   *  A method that should be called from every well-designed equals method
   *  that is open to be overridden in a subclass. See
   *  [[https://www.artima.com/pins1ed/object-equality.html Programming in Scala,
   *  Chapter 28]] for discussion and design.
   *
   *  @param    that    the value being probed for possible equality
   *  @return   true if this instance can possibly equal `that`, otherwise false
   */
  def canEqual(that: Any): Boolean

  /** Checks whether this instance is equal to `that`.
   *  This universal equality method is defined in `AnyRef`.
   */
  def equals(that: Any): Boolean
}
