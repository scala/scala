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

import scala.collection.immutable.Range

// Note that this does not implement IntegralProxy[Int] so that it can return
// the Int-specific Range class from until/to.
final class RichInt(val self: Int) extends AnyVal with ScalaNumberProxy[Int] with RangedProxy[Int] {
  protected def num: scala.math.Numeric.IntIsIntegral.type = scala.math.Numeric.IntIsIntegral
  protected def ord: scala.math.Ordering.Int.type = scala.math.Ordering.Int

  override def doubleValue = self.toDouble
  override def floatValue  = self.toFloat
  override def longValue   = self.toLong
  override def intValue    = self
  override def byteValue   = self.toByte
  override def shortValue  = self.toShort

  /** Returns `'''true'''` if this number has no decimal component.
    * Always `'''true'''` for `RichInt`.
    */
  @deprecated("isWhole on an integer type is always true", "2.12.15")
  def isWhole = true

  override def isValidInt   = true
  def isValidLong  = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  override def abs: Int            = math.abs(self)
  override def max(that: Int): Int = math.max(self, that)
  override def min(that: Int): Int = math.min(self, that)

  /** There is no reason to round an `Int`, but this method is provided to avoid accidental loss of precision from a detour through `Float`. */
  @deprecated("this is an integer type; there is no reason to round it.  Perhaps you meant to call this on a floating-point value?", "2.11.0")
  def round: Int = self

  def toBinaryString: String = java.lang.Integer.toBinaryString(self)
  def toHexString: String    = java.lang.Integer.toHexString(self)
  def toOctalString: String  = java.lang.Integer.toOctalString(self)

  type ResultWithoutStep = Range

  /**
    * @param end The final bound of the range to make.
    * @return A [[scala.collection.immutable.Range]] from `this` up to but
    *         not including `end`.
    */
  def until(end: Int): Range = Range(self, end)

  /**
    * @param end The final bound of the range to make.
    * @param step The number to increase by for each step of the range.
    * @return A [[scala.collection.immutable.Range]] from `this` up to but
    *         not including `end`.
    */
  def until(end: Int, step: Int): Range = Range(self, end, step)

  /** like `until`, but includes the last index */
  /**
    * @param end The final bound of the range to make.
    * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
    *         and including `end`.
    */
  def to(end: Int): Range.Inclusive = Range.inclusive(self, end)

  /**
    * @param end The final bound of the range to make.
    * @param step The number to increase by for each step of the range.
    * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
    *         and including `end`.
    */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self, end, step)
}
