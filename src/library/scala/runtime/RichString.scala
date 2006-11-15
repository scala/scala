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
import compat.Math

final class RichString(s: String) extends AnyRef with Seq[Char] with Ordered[String] with Proxy {

  // Proxy
  def self: Any = s

  // Ordered[String]
  def compare(other: String) = s compareTo other

  // Seq[Char]
  def length = s.length()
  def elements = Iterator.fromString(s)

  /** Retrieve the n-th character of the string
   *
   *  @param   index into the string
   *  @return  the character at position <code>index</code>.
   */
  def apply(n: Int) = s charAt n

  private final val LF: Char = 0x0A
  private final val FF: Char = 0x0C
  private final val CR: Char = 0x0D
  private final val SU: Char = 0x1A

  private def isLineBreak(c: Char) = c == LF || c == FF

  /** <p>
   *    Strip trailing line end character from this string if it has one.
   *    A line end character is one of
   *  </p>
   *  <ul style="list-style-type: none;">
   *    <li>LF - line feed   (0x0A hex)</li>
   *    <li>FF - form feed   (0x0C hex)</li>
   *  </ul>
   *  <p>
   *    If a line feed character LF is preceded by a carriage return CR
   *    (0x0D hex), the CR character is also stripped (Windows convention).
   *  </p>
   */
  def stripLineEnd: String = {
    val len = s.length
    if (len == 0) s
    else {
      val last = apply(len - 1)
      if (isLineBreak(last))
        s.substring(0, if (last == LF && len >= 2 && apply(len - 2) == CR) len - 2 else len - 1)
      else
        s
    }
  }

  /** <p>
   *    Return all lines in this string in an iterator, including trailing
   *    line end characters.
   *  </p>
   *  <p>
   *    The number of strings returned is one greater than the number of line
   *    end characters in this string. For an empty string, a single empty
   *    line is returned. A line end character is one of
   *  </p>
   *  <ul style="list-style-type: none;">
   *    <li>LF - line feed   (0x0A hex)</li>
   *    <li>FF - form feed   (0x0C hex)</li>
   *  </ul>
   */
  def linesWithSeparators = new Iterator[String] {
    val len = s.length
    var index = 0
    def hasNext: Boolean = index <= len
    def next: String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index = index + 1
      index = index + 1
      s.substring(start, Math.min(index, len))
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e. apply <code>.stripLineEnd</code> to all lines
   *  returned by <code>linesWithSeparators</code>.
   */
  def lines = linesWithSeparators map (line => new RichString(line).stripLineEnd)

  /** <p>
   *    For every line in this string:
   *  </p>
   *  <blockquote>
   *     Strip a leading prefix consisting of blanks or control characters
   *     followed by <code>marginChar</code> from the line.
   *  </blockquote>
   */
  def stripMargin(marginChar: Char): String = {
    val buf = new scala.compat.StringBuilder()
    for (val line <- linesWithSeparators) {
      val len = line.length
      var index = 0;
      while (index < len && line.charAt(index) <= ' ') index = index + 1
      buf append
        (if (index < len && line.charAt(index) == marginChar) line.substring(index + 1) else line)
    }
    buf.toString
  }

  /** <p>
   *    For every line in this string:
   *  </p>
   *  <blockquote>
   *     Strip a leading prefix consisting of blanks or control characters
   *     followed by <code>|</code> from the line.
   *  </blockquote>
   */
  def stripMargin: String = stripMargin('|')

  def split(separator: Char): Array[String] = s.split(separator.toString())

  def toByte: Byte = java.lang.Byte.parseByte(s)
  def toShort: Short = java.lang.Short.parseShort(s)
  def toInt: Int = java.lang.Integer.parseInt(s)
  def toLong: Long = java.lang.Long.parseLong(s)
  def toFloat: Float = java.lang.Float.parseFloat(s)
  def toDouble: Double = java.lang.Double.parseDouble(s)

}
