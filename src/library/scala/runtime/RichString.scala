/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RichInt.scala 7771 2006-06-12 13:22:39Z dubochet $
package scala.runtime

final class RichString(s: String) {

  private final val LF: Char = 0x0A
  private final val FF: Char = 0x0C
  private final val CR: Char = 0x0D
  private final val SU: Char = 0x1A

  private def isLineBreak(c: Char) = c == LF || c == FF

  /** Treat string as a function that maps indices to characters
   */
  def apply(index: Int): Char = s charAt index

  /** Strip trailing line end characters from this string
   *  A line end character is one of
   *   LF - line feed   (0x0A hex)
   *   FF - form feed   (0x0C hex)
   *  If a line feed character LF is preceded by a carriage return CR (0x0D hex),
   *  the CR character is also stripped (Windows convention)
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

  /** Return all lines in this string in an iterator, including trailing line end characters
   *  The number of strings returned is one greater than the number of line end characters
   *  in this string. For an empty string, a single empty line is returned.
   *  A line end character is one of
   *   LF - line feed   (0x0A hex)
   *   FF - form feed   (0x0C hex)
   */
  def linesWithSeparators = new Iterator[String] {
    val len = s.length
    var index = 0
    def hasNext: Boolean = index <= len
    def next: String = {
      if (index >= len) Predef.error("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index = index + 1
      index = index + 1
      s.substring(start, Math.min(index, len))
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line end characters
   *  I.e. apply `.stripLineEnd' to all lines returned by `linesWithSeparators'
   */
  def lines = linesWithSeparators map (line => new RichString(line).stripLineEnd)

  /** For every line in this string:
   *     Strip a leading prefix consisting of blanks or control characters followed by
   *     `marginChar' from the line.
   */
  def stripMargin(marginChar: Char): String = {
    val buf = new StringBuffer()
    for (val line <- linesWithSeparators) {
      val len = line.length
      var index = 0;
      while (index < len && line.charAt(index) <= ' ') index = index + 1
      buf append
        (if (index < len && line.charAt(index) == marginChar) line.substring(index + 1) else line)
    }
    buf.toString
  }

  /** For every line in this string:
   *     Strip a leading prefix consisting of blanks or control characters followed by
   *     `|' from the line.
   */
  def stripMargin: String = stripMargin('|')
}

