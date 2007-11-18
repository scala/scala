/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._

final class RichString(val self: String) extends Proxy with RandomAccessSeq[Char] with Ordered[String] {
  import RichString._
  override def apply(n: Int) = self charAt n
  override def length = self.length
  override def toString = self
  override def mkString = self

  override def slice(from: Int, until: Int): RichString = {
    val len = self.length
    new RichString(
      if (from >= until || from >= len)
        ""
      else {
        val from0 = if (from < 0) 0 else from
        val until0 = if (until > len) len else until
        self.substring(from0, until0)
      }
    )
  }

  //override def ++ [B >: A](that: Iterable[B]): Seq[B] = {
  override def ++[B >: Char](that: Iterable[B]): RandomAccessSeq[B] = that match {
    case that: RichString => new RichString(self + that.self)
    case that => super.++(that)
  }

  override def take(until: Int): RichString = slice(0, until)

  override def drop(from: Int): RichString = slice(from, self.length)

  override def startsWith[B](that: Seq[B]) = that match {
    case that: RichString => self startsWith that.self
    case that => super.startsWith(that)
  }

  override def endsWith[B](that: Seq[B]) = that match {
    case that: RichString => self endsWith that.self
    case that => super.endsWith(that)
  }

  override def indexOf[B](that: Seq[B]) = that match {
    case that: RichString => self indexOf that.self
    case that => super.indexOf(that)
  }

  override def containsSlice[B](that: Seq[B]) = that match {
    case that: RichString => self contains that.self
    case that => super.containsSlice(that)
  }

  override def compare(other: String) = self compareTo other

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
    def hasNext: Boolean = index < len
    def next(): String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index += 1
      index += 1
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
    val buf = new StringBuilder()
    for (line <- linesWithSeparators) {
      val len = line.length
      var index = 0
      while (index < len && line.charAt(index) <= ' ') index += 1
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
/*
  private def escape(ch: Char): String = ch match {
    case '.' | '$' | '^' | '\\' => "\\" + ch
    case _ => "" + ch
  }

  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separator: Char): Array[String] = self.split(escape(separator))

  @throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+_) + "]"
    self.split(re)
  }
*/
  def toByte: Byte     = java.lang.Byte.parseByte(self)
  def toShort: Short   = java.lang.Short.parseShort(self)
  def toInt: Int       = java.lang.Integer.parseInt(self)
  def toLong: Long     = java.lang.Long.parseLong(self)
  //def toFloat: Float   = java.lang.Float.parseFloat(self)
  //def toDouble: Double = java.lang.Double.parseDouble(self)
}

object RichString {
  // just statics for rich string.
  private final val LF: Char = 0x0A
  private final val FF: Char = 0x0C
  private final val CR: Char = 0x0D
  private final val SU: Char = 0x1A
}

