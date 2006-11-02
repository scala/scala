/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime

import java.lang.Character

final class RichChar(c: Char) {

  def asDigit: Int = Character.digit(c, Character.MAX_RADIX)
  def asDigit(radix: Int): Int = Character.digit(c, radix)

  def isControl: Boolean = Character.isISOControl(c)
  def isDigit: Boolean = Character.isDigit(c)
  def isISOControl: Boolean = Character.isISOControl(c)
  def isLetter: Boolean = Character.isLetter(c)
  def isLetterOrDigit: Boolean = Character.isLetterOrDigit(c)
  def isLowerCase: Boolean = Character.isLowerCase(c)
  def isUpperCase: Boolean = Character.isUpperCase(c)
  def isWhitespace: Boolean = Character.isWhitespace(c)

  def toLowerCase: Char = Character.toLowerCase(c)
  def toUpperCase: Char = Character.toUpperCase(c)

}
