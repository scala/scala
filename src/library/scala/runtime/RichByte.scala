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

final class RichByte(private val self: Byte) extends AnyVal {
  @deprecated("statically known to be true", "2.14.0")
  def isWhole: Boolean = true

  @deprecated("use the method available on Byte itself", "2.14.0")
  def toChar: Char = self.toChar
  @deprecated("use the method available on Byte itself", "2.14.0")
  def toByte: Byte = self.toByte
  @deprecated("use the method available on Byte itself", "2.14.0")
  def toShort: Short = self.toShort
  @deprecated("use the method available on Byte itself", "2.14.0")
  def toInt: Int = self.toInt
  @deprecated("use the method available on Byte itself", "2.14.0")
  def toLong: Long = self.toLong
  @deprecated("use the method available on Byte itself", "2.14.0")
  def toFloat: Float = self.toFloat
  @deprecated("use the method available on Byte itself", "2.14.0")
  def toDouble: Double = self.toDouble

  @deprecated("statically known to be an identity", "2.14.0")
  def byteValue: Byte = self
  @deprecated("use toShort instead", "2.14.0")
  def shortValue: Short = self.toShort
  @deprecated("use toInt instead", "2.14.0")
  def intValue: Int = self.toInt
  @deprecated("use toLong instead", "2.14.0")
  def longValue: Long = self.toLong
  @deprecated("use toFloat instead", "2.14.0")
  def floatValue: Float = self.toFloat
  @deprecated("use toDouble instead", "2.14.0")
  def doubleValue: Double = self.toDouble

  /** Returns `true`. */
  @deprecated("statically known to be true", "2.14.0")
  def isValidByte: Boolean = true

  /** Returns `true`. */
  @deprecated("statically known to be true", "2.14.0")
  def isValidShort: Boolean = true

  /** Returns `true`. */
  @deprecated("statically known to be true", "2.14.0")
  def isValidInt: Boolean = true

  /** Returns `true` if this is within the range of [[scala.Char]] MinValue
   * and MaxValue; otherwise returns `false`.
   */
  def isValidChar: Boolean = self >= 0

  /** Result of comparing `this` with operand `that`.
   *
   * Returns `x` where:
   *
   *  - `x < 0` when `this < that`
   *  - `x == 0` when `this == that`
   *  - `x > 0` when  `this > that`
   */
  def compare(y: Byte): Int = java.lang.Byte.compare(self, y)

  /** Returns `'''this'''` if `'''this''' < that` or `that` otherwise. */
  def min(that: Byte): Byte = Math.min(self, that).toByte

  /** Returns `'''this'''` if `'''this''' > that` or `that` otherwise. */
  def max(that: Byte): Byte = Math.max(self, that).toByte

  /** Returns the absolute value of `'''this'''`. */
  def abs: Byte = Math.abs(self).toByte

  /**
   * Returns the sign of `'''this'''`.
   * zero if the argument is zero, -zero if the argument is -zero,
   * one if the argument is greater than zero, -one if the argument is less than zero,
   * and NaN if the argument is NaN where applicable.
   */
  def sign: Byte = Integer.signum(self).toByte

  /** Returns the signum of `'''this'''`. */
  @deprecated("use `sign` method instead", since = "2.13.0")
  def signum: Int = sign

  override def toString(): String = java.lang.Byte.toString(self)
}
