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
package runtime

import scala.collection.immutable.{NumericRange, Range}

final class RichLong(private val self: Long) extends AnyVal {
  @deprecated("statically known to be true", "2.14.0")
  def isWhole: Boolean = true

  @deprecated("use the method available on Int itself", "2.14.0")
  def toChar: Char = self.toChar
  @deprecated("use the method available on Int itself", "2.14.0")
  def toByte: Byte = self.toByte
  @deprecated("use the method available on Int itself", "2.14.0")
  def toShort: Short = self.toShort
  @deprecated("use the method available on Int itself", "2.14.0")
  def toInt: Int = self.toInt
  @deprecated("use the method available on Int itself", "2.14.0")
  def toLong: Long = self.toLong
  @deprecated("use the method available on Int itself", "2.14.0")
  def toFloat: Float = self.toFloat
  @deprecated("use the method available on Int itself", "2.14.0")
  def toDouble: Double = self.toDouble

  @deprecated("use toByte instead", "2.14.0")
  def byteValue: Byte = self.toByte
  @deprecated("use toShort instead", "2.14.0")
  def shortValue: Short = self.toShort
  @deprecated("use toLong instead", "2.14.0")
  def intValue: Int = self.toInt
  @deprecated("statically known to be an identity", "2.14.0")
  def longValue: Long = self
  @deprecated("use toFloat instead", "2.14.0")
  def floatValue: Float = self.toFloat
  @deprecated("use toDouble instead", "2.14.0")
  def doubleValue: Double = self.toDouble


  /** Returns `true` if this is within the range of [[scala.Byte]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidByte: Boolean = self.toByte.toLong == self

  /** Returns `true` if this is within the range of [[scala.Short]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidShort: Boolean = self.toShort.toLong == self

  /** Returns `true` if this is within the range of [[scala.Int]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidInt: Boolean = self.toInt.toLong == self

  /** Returns `true` if this is within the range of [[scala.Char]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidChar: Boolean = self.toChar.toLong == self

  def isValidLong = true

  def abs: Long = Math.abs(self)
  def max(that: Long): Long = Math.max(self, that)
  def min(that: Long): Long = Math.min(self, that)

  /** There is no reason to round a `Long`, but this method is provided to avoid accidental conversion to `Int` through `Float`. */
  @deprecated("this is an integer type; there is no reason to round it.  Perhaps you meant to call this on a floating-point value?", "2.11.0")
  def round: Long = self

  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  def toHexString: String = java.lang.Long.toHexString(self)
  def toOctalString: String = java.lang.Long.toOctalString(self)

  @deprecated("use Range instead", "2.14.0")
  type ResultWithoutStep = NumericRange[Long]

  /** Create a `NumericRange[Long]` in range `[start;end)`
   * with the specified step, where start is the target Long.
   *
   * @param end The final bound of the range to make.
   * @return the range
   */
  def until(end: Long): NumericRange.Exclusive[Long] = Range.Long(self, end, 1)

  /** Create a `NumericRange[Long]` in range `[start;end]`
   * with the specified step, where start is the target Long.
   *
   * @param end  The final bound of the range to make.
   * @param step The number to increase by for each step of the range.
   * @return the range
   */
  def until(end: Long, step: Long): NumericRange.Exclusive[Long] = Range.Long(self, end, step)

  /** like `until`, but includes the last index.
   *
   * @param end The final bound of the range to make.
   * @return the range
   */
  def to(end: Long): NumericRange.Inclusive[Long] = Range.Long.inclusive(self, end, 1)

  /** like `until`, but includes the last index.
   *
   * @param end  The final bound of the range to make.
   * @param step The number to increase by for each step of the range.
   * @return the range
   */
  def to(end: Long, step: Long): NumericRange.Inclusive[Long] = Range.Long.inclusive(self, end, step)
}
