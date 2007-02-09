/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

import Predef.NoSuchElementException

final class RichChar(x: Char) extends Proxy with Ordered[Char] {
  // Proxy.self
  def self: Any = x

  // Ordered[Char].compare
  def compare (y: Char): Int = if (x < y) -1 else if (x > y) 1 else 0

  def isControl: Boolean = System.Char.IsControl(x)
  def isDigit: Boolean = System.Char.IsDigit(x)
  def isLetter: Boolean = System.Char.IsLetter(x)
  def isLetterOrDigit: Boolean = System.Char.IsLetterOrDigit(x)
  def isLowerCase: Boolean = System.Char.IsLower(x)
  def isUpperCase: Boolean = System.Char.IsUpper(x)
  def isWhitespace: Boolean = System.Char.IsWhiteSpace(x)
  def toLowerCase: Char = System.Char.ToLower(x)
  def toUpperCase: Char = System.Char.ToUpper(x)

  def asDigit: Int = System.Char.GetNumericValue(x).toInt

  def to(y: Char): Iterator[Char] = new BufferedIterator[Char] {
    private var ch = x
    def hasNext: Boolean = ch < y
    def next: Char =
      if (hasNext) { val j = ch; ch = (ch + 1).toChar; j }
      else throw new NoSuchElementException("next on empty iterator")
    def head: Char =
      if (hasNext) ch
      else throw new NoSuchElementException("head on empty iterator")
  }

}
