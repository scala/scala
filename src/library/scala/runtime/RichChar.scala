/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime

import Predef._

final class RichChar(c: Char) {
  def asDigit: Int = Character.digit(c, 10)

  def isControl: Boolean = Character.isISOControl(c)
  def isDigit: Boolean = Character.isDigit(c)
  def isISOControl: Boolean = Character.isISOControl(c)
  def isLetter: Boolean = Character.isLetter(c)
  def isLetterOrDigit: Boolean = Character.isLetterOrDigit(c)
  def isLowerCase: Boolean = Character.isLowerCase(c)
  def isSpaceChar: Boolean = Character.isSpaceChar(c)
  def isTitleCase: Boolean = Character.isTitleCase(c)
  def isUnicodeIdentifierPart: Boolean = Character.isUnicodeIdentifierPart(c)
  def isUnicodeIdentifierStart: Boolean = Character.isUnicodeIdentifierStart(c)
  def isUpperCase: Boolean = Character.isUpperCase(c)
  def isWhitespace: Boolean = Character.isWhitespace(c)

  def toLowerCase: Char = Character.toLowerCase(c)
  def toTitleCase: Char = Character.toTitleCase(c)
  def toUpperCase: Char = Character.toUpperCase(c)
}
