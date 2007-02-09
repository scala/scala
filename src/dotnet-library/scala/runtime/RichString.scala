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

final class RichString(val self: String) extends Proxy with Seq[Char] with Ordered[String] {

  // Ordered[String]
  def compare(other: String) = self compareTo other

  // Seq[Char]
  def length = self.length()
  def elements = Iterator.fromString(self)

  /** Retrieve the n-th character of the string
   *
   *  @param   index into the string
   *  @return  the character at position <code>index</code>.
   */
  def apply(n: Int) = self charAt n

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
    val len = self.length
    if (len == 0) self
    else {
      val last = apply(len - 1)
      if (isLineBreak(last))
        self.substring(0, if (last == LF && len >= 2 && apply(len - 2) == CR) len - 2 else len - 1)
      else
        self
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
    val len = self.length
    var index = 0
    def hasNext: Boolean = index <= len
    def next: String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index = index + 1
      index = index + 1
      self.substring(start, index min len)
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e. apply <code>.stripLineEnd</code> to all lines
   *  returned by <code>linesWithSeparators</code>.
   */
  def lines: Iterator[String] =
    linesWithSeparators map (line => new RichString(line).stripLineEnd)

  /** Returns this string with first character converted to upper case */
  def capitalize: String = {
    val chars = self.toCharArray
    chars(0) = chars(0).toUpperCase
    new String(chars)
  }

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

  def split(separator: Char): Array[String] = self.Split(Array(separator))

  def toByte: Byte = System.Byte.Parse(self)
  def toShort: Short = System.Int16.Parse(self)
  def toInt: Int = System.Int32.Parse(self)
  def toLong: Long = System.Int64.Parse(self)
  def toFloat: Float = System.Single.Parse(self)
  def toDouble: Double = System.Double.Parse(self)
}
