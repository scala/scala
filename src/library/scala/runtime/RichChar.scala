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

final class RichChar(val self: Char) extends AnyVal with IntegralProxy[Char] {
  protected def num: scala.math.Numeric.CharIsIntegral.type = scala.math.Numeric.CharIsIntegral
  protected def ord: scala.math.Ordering.Char.type = scala.math.Ordering.Char

  override def doubleValue = self.toDouble
  override def floatValue  = self.toFloat
  override def longValue   = self.toLong
  override def intValue    = self.toInt
  override def byteValue   = self.toByte
  override def shortValue  = self.toShort

  override def isValidChar   = true

  // These method are all overridden and redefined to call out to scala.math to avoid 3 allocations:
  // the primitive boxing, the value class boxing and instantiation of the Numeric num.
  // We'd like to redefine signum and sign too but forwards binary compatibility doesn't allow us to.
  override def abs: Char             = self
  override def max(that: Char): Char = math.max(self.toInt, that.toInt).toChar
  override def min(that: Char): Char = math.min(self.toInt, that.toInt).toChar

  def asDigit: Int                      = Character.digit(self, Character.MAX_RADIX)

  def isControl: Boolean                = Character.isISOControl(self)
  def isDigit: Boolean                  = Character.isDigit(self)
  def isLetter: Boolean                 = Character.isLetter(self)
  def isLetterOrDigit: Boolean          = Character.isLetterOrDigit(self)
  def isWhitespace: Boolean             = Character.isWhitespace(self)
  def isSpaceChar: Boolean              = Character.isSpaceChar(self)
  def isHighSurrogate: Boolean          = Character.isHighSurrogate(self)
  def isLowSurrogate: Boolean           = Character.isLowSurrogate(self)
  def isSurrogate: Boolean              = isHighSurrogate || isLowSurrogate
  def isUnicodeIdentifierStart: Boolean = Character.isUnicodeIdentifierStart(self)
  def isUnicodeIdentifierPart: Boolean  = Character.isUnicodeIdentifierPart(self)
  def isIdentifierIgnorable: Boolean    = Character.isIdentifierIgnorable(self)
  def isMirrored: Boolean               = Character.isMirrored(self)

  def isLower: Boolean                  = Character.isLowerCase(self)
  def isUpper: Boolean                  = Character.isUpperCase(self)
  def isTitleCase: Boolean              = Character.isTitleCase(self)

  def toLower: Char                     = Character.toLowerCase(self)
  def toUpper: Char                     = Character.toUpperCase(self)
  def toTitleCase: Char                 = Character.toTitleCase(self)

  def getType: Int                      = Character.getType(self)
  def getNumericValue: Int              = Character.getNumericValue(self)
  def getDirectionality: Byte           = Character.getDirectionality(self)
  def reverseBytes: Char                = Character.reverseBytes(self)

  // Java 5 Character methods not added:
  //
  // public static boolean isDefined(char ch)
  // public static boolean isJavaIdentifierStart(char ch)
  // public static boolean isJavaIdentifierPart(char ch)
}
