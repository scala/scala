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

final class RichChar(val self: Char) extends AnyVal {
  @deprecated("statically known to be true", "2.14.0")
  def isWhole = true

  @deprecated("use the method available on Char itself", "2.14.0")
  def toChar: Char = self.toChar
  @deprecated("use the method available on Char itself", "2.14.0")
  def toByte: Byte = self.toByte
  @deprecated("use the method available on Char itself", "2.14.0")
  def toShort: Short = self.toShort
  @deprecated("use the method available on Char itself", "2.14.0")
  def toInt: Int = self.toInt
  @deprecated("use the method available on Char itself", "2.14.0")
  def toLong: Long = self.toLong
  @deprecated("use the method available on Char itself", "2.14.0")
  def toFloat: Float = self.toFloat
  @deprecated("use the method available on Char itself", "2.14.0")
  def toDouble: Double = self.toDouble

  @deprecated("use toByte instead", "2.14.0")
  def byteValue: Byte = self.toByte
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

  def abs: Char = self
  def max(that: Char): Char = Math.max(self.toInt, that.toInt).toChar
  def min(that: Char): Char = Math.min(self.toInt, that.toInt).toChar

  def asDigit: Int = Character.digit(self, Character.MAX_RADIX)

  def isControl: Boolean = Character.isISOControl(self)
  def isDigit: Boolean = Character.isDigit(self)
  def isLetter: Boolean = Character.isLetter(self)
  def isLetterOrDigit: Boolean = Character.isLetterOrDigit(self)
  def isWhitespace: Boolean = Character.isWhitespace(self)
  def isSpaceChar: Boolean = Character.isSpaceChar(self)
  def isHighSurrogate: Boolean = Character.isHighSurrogate(self)
  def isLowSurrogate: Boolean = Character.isLowSurrogate(self)
  def isSurrogate: Boolean = isHighSurrogate || isLowSurrogate
  def isUnicodeIdentifierStart: Boolean = Character.isUnicodeIdentifierStart(self)
  def isUnicodeIdentifierPart: Boolean = Character.isUnicodeIdentifierPart(self)
  def isIdentifierIgnorable: Boolean = Character.isIdentifierIgnorable(self)
  def isMirrored: Boolean = Character.isMirrored(self)

  def compare(y: Char): Int = java.lang.Character.compare(self, y)

  def isLower: Boolean = Character.isLowerCase(self)
  def isUpper: Boolean = Character.isUpperCase(self)
  def isTitleCase: Boolean = Character.isTitleCase(self)

  def toLower: Char = Character.toLowerCase(self)
  def toUpper: Char = Character.toUpperCase(self)
  def toTitleCase: Char = Character.toTitleCase(self)

  def getType: Int = Character.getType(self)
  def getNumericValue: Int = Character.getNumericValue(self)
  def getDirectionality: Byte = Character.getDirectionality(self)
  def reverseBytes: Char = Character.reverseBytes(self)

  @deprecated("use Range instead", "2.14.0")
  type ResultWithoutStep = Range

  /**
   * @param end The final bound of the range to make.
   * @return A [[scala.collection.immutable.Range]] from `this` up to but
   *         not including `end`.
   */
  def until(end: Char): Range = Range(self.toInt, end.toInt)

  /**
   * @param end  The final bound of the range to make.
   * @param step The number to increase by for each step of the range.
   * @return A [[scala.collection.immutable.Range]] from `this` up to but
   *         not including `end`.
   */
  def until(end: Char, step: Char): Range = Range(self.toInt, end.toInt, step.toInt)

  /** like `until`, but includes the last index.
   *
   * @param end The final bound of the range to make.
   * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
   *         and including `end`.
   */
  def to(end: Char): Range.Inclusive = Range.inclusive(self.toInt, end.toInt)

  /** like `until`, but includes the last index.
   *
   * @param end  The final bound of the range to make.
   * @param step The number to increase by for each step of the range.
   * @return A [[scala.collection.immutable.Range]] from `'''this'''` up to
   *         and including `end`.
   */
  def to(end: Char, step: Char): Range.Inclusive = Range.inclusive(self.toInt, end.toInt, step.toInt)

  /**
   * Returns the sign of `'''this'''`.
   * zero if the argument is zero, -zero if the argument is -zero,
   * one if the argument is greater than zero, -one if the argument is less than zero,
   * and NaN if the argument is NaN where applicable.
   */
  def sign: Char = Integer.signum(self.toInt).toChar

  /** Returns the signum of `'''this'''`. */
  @deprecated("use `sign` method instead", since = "2.13.0")
  def signum: Int = sign

  override def toString(): String = java.lang.Character.toString(self)

  // Java 5 Character methods not added:
  //
  // public static boolean isDefined(char ch)
  // public static boolean isJavaIdentifierStart(char ch)
  // public static boolean isJavaIdentifierPart(char ch)
}
