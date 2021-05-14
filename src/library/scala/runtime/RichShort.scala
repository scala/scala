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

final class RichShort(private val self: Short) extends AnyVal {
  @deprecated("statically known to be true", "2.14.0")
  def isWhole: Boolean = true

  @deprecated("use the method available on Short itself", "2.14.0")
  def toChar: Char = self.toChar
  @deprecated("use the method available on Short itself", "2.14.0")
  def toByte: Byte = self.toByte
  @deprecated("use the method available on Short itself", "2.14.0")
  def toShort: Short = self.toShort
  @deprecated("use the method available on Short itself", "2.14.0")
  def toInt: Int = self.toInt
  @deprecated("use the method available on Short itself", "2.14.0")
  def toLong: Long = self.toLong
  @deprecated("use the method available on Short itself", "2.14.0")
  def toFloat: Float = self.toFloat
  @deprecated("use the method available on Short itself", "2.14.0")
  def toDouble: Double = self.toDouble

  @deprecated("use toByte instead", "2.14.0")
  def byteValue: Byte = self.toByte
  @deprecated("statically known to be an identity", "2.14.0")
  def shortValue: Short = self
  @deprecated("use toLong instead", "2.14.0")
  def intValue: Int = self.toInt
  @deprecated("use toShort instead", "2.14.0")
  def longValue: Long = self.toLong
  @deprecated("use toFloat instead", "2.14.0")
  def floatValue: Float = self.toFloat
  @deprecated("use toDouble instead", "2.14.0")
  def doubleValue: Double = self.toDouble

  def isValidShort: Boolean = true

  def abs: Short = Math.abs(self.toInt).toShort
  def max(that: Short): Short = Math.max(self.toInt, that.toInt).toShort
  def min(that: Short): Short = Math.min(self.toInt, that.toInt).toShort

  /**
   * Returns the sign of `'''this'''`.
   * zero if the argument is zero, -zero if the argument is -zero,
   * one if the argument is greater than zero, -one if the argument is less than zero,
   * and NaN if the argument is NaN where applicable.
   */
  def sign: Short = Integer.signum(self.toInt).toShort

  /** Returns the signum of `'''this'''`. */
  @deprecated("use `sign` method instead", since = "2.13.0")
  def signum: Int = sign

  override def toString(): String = java.lang.Short.toString(self)
}
