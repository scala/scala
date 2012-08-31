/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import java.lang.Character

final class RichChar(val self: Char) extends AnyVal with IntegralProxy[Char] {
  protected def num = scala.math.Numeric.CharIsIntegral
  protected def ord = scala.math.Ordering.Char

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
