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

import scala.collection.immutable.Range

final class RichInt(private val self: Int) extends AnyVal {
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
  @deprecated("statically known to be an identity", "2.14.0")
  def intValue: Int = self
  @deprecated("use toLong instead", "2.14.0")
  def longValue: Long = self.toLong
  @deprecated("use toFloat instead", "2.14.0")
  def floatValue: Float = self.toFloat
  @deprecated("use toDouble instead", "2.14.0")
  def doubleValue: Double = self.toDouble

  /** Returns `true` if this is within the range of [[scala.Byte]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidByte: Boolean = self.toByte.toInt == self

  /** Returns `true` if this is within the range of [[scala.Short]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidShort: Boolean = self.toShort.toInt == self

  /** Returns `true`. */
  @deprecated("statically known to be true", "2.14.0")
  def isValidInt: Boolean = true

  /** Returns `true` if this is within the range of [[scala.Char]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidChar: Boolean = self.toChar.toInt == self

  /** Result of comparing `this` with operand `that`.
   *
   * Returns `x` where:
   *
   *  - `x < 0` when `this < that`
   *  - `x == 0` when `this == that`
   *  - `x > 0` when  `this > that`
   */
  def compare(y: Int): Int = Integer.compare(self, y)

  /** Returns `'''this'''` if `'''this''' < that` or `that` otherwise. */
  def min(that: Int): Int = Math.min(self, that)
  /** Returns `'''this'''` if `'''this''' > that` or `that` otherwise. */
  def max(that: Int): Int = Math.max(self, that)

  /** Returns the absolute value of `'''this'''`. */
  def abs: Int = Math.abs(self)

  /**
   * Returns the sign of `'''this'''`.
   * zero if the argument is zero, -zero if the argument is -zero,
   * one if the argument is greater than zero, -one if the argument is less than zero,
   * and NaN if the argument is NaN where applicable.
   */
  def sign: Int = Integer.signum(self)

  /** Returns the signum of `'''this'''`. */
  @deprecated("use `sign` method instead", since = "2.13.0")
  def signum: Int = sign

  /** There is no reason to round an `Int`, but this method is provided to avoid accidental loss of precision from a detour through `Float`. */
  @deprecated("this is an integer type; there is no reason to round it.  Perhaps you meant to call this on a floating-point value?", "2.11.0")
  def round: Int = self

  def toBinaryString: String = java.lang.Integer.toBinaryString(self)
  def toHexString: String = java.lang.Integer.toHexString(self)
  def toOctalString: String = java.lang.Integer.toOctalString(self)

  @deprecated("use Range instead", "2.14.0")
  type ResultWithoutStep = Range

  /**
   * @param end The final bound of the range to make.
   * @return A [[scala.collection.immutable.Range]] from `this` up to but
   *         not including `end`.
   */
  def until(end: Int): Range = Range(self, end)

  /**
   * @param end  The final bound of the range to make.
   * @param step The number to increase by for each step of the range.
   * @return A [[scala.collection.immutable.Range]] from `this` up to but
   *         not including `end`.
   */
  def until(end: Int, step: Int): Range = Range(self, end, step)

  /** like `until`, but includes the last index.
   *
   * @param end The final bound of the range to make.
   * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
   *         and including `end`.
   */
  def to(end: Int): Range.Inclusive = Range.inclusive(self, end)

  /** like `until`, but includes the last index.
   *
   * @param end  The final bound of the range to make.
   * @param step The number to increase by for each step of the range.
   * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
   *         and including `end`.
   */
  def to(end: Int, step: Int): Range.Inclusive = Range.inclusive(self, end, step)

  override def toString(): String = java.lang.Integer.toString(self)
}
