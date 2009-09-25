/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import java.lang.Character
import collection.{Vector, VectorView}
import Predef.{require, NoSuchElementException}

/** <p>
 *    For example, in the following code
 *  </p>
 *  <pre>
 *    <b>object</b> test <b>extends</b> Application {
 *      Console.println(<chr>'\40'</chr>.isWhitespace)
 *      Console.println('\011'.isWhitespace)
 *      Console.println('1'.asDigit == 1)
 *      Console.println('A'.asDigit == 10)
 *    }</pre>
 *  <p>
 *    the implicit conversions are performed using the predefined view
 *    <a href="../Predef$object.html#charWrapper(scala.Char)"
 *    target="contentFrame"><code>Predef.charWrapper</code></a>.
 *  </p>
 */
final class RichChar(x: Char) extends Proxy with Ordered[Char] {

  // Proxy.self
  def self: Any = x

  // Ordered[Char].compare
  def compare (y: Char): Int = if (x < y) -1 else if (x > y) 1 else 0

  def asDigit: Int = Character.digit(x, Character.MAX_RADIX)

  def isControl: Boolean = Character.isISOControl(x)
  def isDigit: Boolean = Character.isDigit(x)
  def isLetter: Boolean = Character.isLetter(x)
  def isLetterOrDigit: Boolean = Character.isLetterOrDigit(x)
  def isWhitespace: Boolean = Character.isWhitespace(x)
  def isSpaceChar: Boolean = Character.isSpaceChar(x)
  def isHighSurrogate: Boolean = Character.isHighSurrogate(x)
  def isLowSurrogate: Boolean = Character.isLowSurrogate(x)
  def isUnicodeIdentifierStart: Boolean = Character.isUnicodeIdentifierStart(x)
  def isUnicodeIdentifierPart: Boolean = Character.isUnicodeIdentifierPart(x)
  def isIdentifierIgnorable: Boolean = Character.isIdentifierIgnorable(x)
  def isMirrored: Boolean = Character.isMirrored(x)

  def isLower: Boolean = Character.isLowerCase(x)
  def isUpper: Boolean = Character.isUpperCase(x)
  def isTitleCase: Boolean = Character.isTitleCase(x)

  def toLower: Char = Character.toLowerCase(x)
  def toUpper: Char = Character.toUpperCase(x)
  def toTitleCase: Char = Character.toTitleCase(x)

  def getType: Int = Character.getType(x)
  def getNumericValue: Int = Character.getNumericValue(x)
  def getDirectionality: Byte = Character.getDirectionality(x)
  def reverseBytes: Char = Character.reverseBytes(x)

  // Java 5 Character methods not added:
  //
  // public static boolean isDefined(char ch)
  // public static boolean isJavaIdentifierStart(char ch)
  // public static boolean isJavaIdentifierPart(char ch)

  @deprecated("Use ch.toLower instead")
  def toLowerCase: Char = toLower
  @deprecated("Use ch.toUpper instead")
  def toUpperCase: Char = toUpper

  @deprecated("Use ch.isLower instead")
  def isLowerCase: Boolean = isLower
  @deprecated("Use ch.isUpper instead")
  def isUpperCase: Boolean = isUpper

  /** Create a <code>[Char]</code> over the characters from 'x' to 'y' - 1
   */
  def until(limit: Char): VectorView[Char, Vector[Char]] =
    if (limit <= x) Vector.empty.view
    else
      new VectorView[Char, Vector[Char]] {
        protected def underlying = Vector.empty[Char]
        def length = limit - x
        def apply(i: Int): Char = {
          require(i >= 0 && i < length)
          (x + i).toChar
        }
      }

  /** Create a <code>VectorView[Char]</code> over the characters from 'x' to 'y'
   */
  def to(y: Char): VectorView[Char, Vector[Char]] = until((y + 1).toChar)

}
