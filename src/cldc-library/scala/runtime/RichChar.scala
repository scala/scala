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
import Predef.NoSuchElementException

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

  //def isControl: Boolean = Character.isISOControl(x)
  def isDigit: Boolean = Character.isDigit(x)
  //def isLetter: Boolean = Character.isLetter(x)
  //def isLetterOrDigit: Boolean = Character.isLetterOrDigit(x)
  def isLowerCase: Boolean = Character.isLowerCase(x)
  def isUpperCase: Boolean = Character.isUpperCase(x)
  //def isWhitespace: Boolean = Character.isWhitespace(x)

  def toLowerCase: Char = Character.toLowerCase(x)
  def toUpperCase: Char = Character.toUpperCase(x)

  /** Create an Iterator[Char] over the characters from 'x' to 'y' - 1
   */
  def until(limit: Char): Iterator[Char] = new Iterator[Char] {
    private var ch = x
    def hasNext: Boolean = ch < limit
    def next: Char =
      if (hasNext) { val j = ch; ch = (ch + 1).toChar; j }
      else throw new NoSuchElementException("next on empty iterator")
  }

  //def until(y: Char): Iterator[Char] = to(y)

  /** Create an Iterator[Char] over the characters from 'x' to 'y'
   */
  def to(y: Char): Iterator[Char] = until((y + 1).toChar)

}
