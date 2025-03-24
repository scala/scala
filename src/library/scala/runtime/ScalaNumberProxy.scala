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
package runtime

import scala.collection.immutable
import scala.math.ScalaNumericAnyConversions
import immutable.NumericRange
import Proxy.Typed
import scala.annotation.nowarn

/** Base classes for the Rich* wrappers of the primitive types.
 *  As with all classes in scala.runtime.*, this is not a supported API.
 */
@nowarn("cat=deprecation")
trait ScalaNumberProxy[T] extends Any with ScalaNumericAnyConversions with Typed[T] with OrderedProxy[T] {
  protected implicit def num: Numeric[T]

  def doubleValue = num.toDouble(self)
  def floatValue  = num.toFloat(self)
  def longValue   = num.toLong(self)
  def intValue    = num.toInt(self)
  def byteValue   = intValue.toByte
  def shortValue  = intValue.toShort

  /** Returns `'''this'''` if `'''this''' < that` or `that` otherwise. */
  def min(that: T): T = num.min(self, that)
  /** Returns `'''this'''` if `'''this''' > that` or `that` otherwise. */
  def max(that: T): T = num.max(self, that)
  /** Returns the absolute value of `'''this'''`. */
  def abs             = num.abs(self)
  /**
   * Returns the sign of `'''this'''`.
   * zero if the argument is zero, -zero if the argument is -zero,
   * one if the argument is greater than zero, -one if the argument is less than zero,
   * and NaN if the argument is NaN where applicable.
   */
  def sign: T         = num.sign(self)
  /** Returns the signum of `'''this'''`. */
  @deprecated("use `sign` method instead", since = "2.13.0") def signum: Int = num.signum(self)
}
trait ScalaWholeNumberProxy[T] extends Any with ScalaNumberProxy[T] {
  @deprecated("isWhole on an integer type is always true", "2.12.15")
  def isWhole = true
}
trait IntegralProxy[T] extends Any with ScalaWholeNumberProxy[T] with RangedProxy[T] {
  protected implicit def num: Integral[T]
  type ResultWithoutStep = NumericRange[T]

  def until(end: T): NumericRange.Exclusive[T]          = NumericRange(self, end, num.one)
  def until(end: T, step: T): NumericRange.Exclusive[T] = NumericRange(self, end, step)
  def to(end: T): NumericRange.Inclusive[T]             = NumericRange.inclusive(self, end, num.one)
  def to(end: T, step: T): NumericRange.Inclusive[T]    = NumericRange.inclusive(self, end, step)
}
trait FractionalProxy[T] extends Any with ScalaNumberProxy[T] {
  protected implicit def num: Fractional[T]

  def isWhole = false
}

@nowarn("cat=deprecation")
trait OrderedProxy[T] extends Any with Ordered[T] with Typed[T] {
  protected def ord: Ordering[T]

  def compare(y: T) = ord.compare(self, y)
}

@nowarn("cat=deprecation")
trait RangedProxy[T] extends Any with Typed[T] {
  type ResultWithoutStep

  def until(end: T): ResultWithoutStep
  def until(end: T, step: T): immutable.IndexedSeq[T]
  def to(end: T): ResultWithoutStep
  def to(end: T, step: T): immutable.IndexedSeq[T]
}

