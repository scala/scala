/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml

import Utility.isSpace

object TextBuffer {
  def fromString(str: String): TextBuffer = new TextBuffer() append str
}

/** The class `TextBuffer` is for creating text nodes without surplus
 *  whitespace. All occurrences of one or more whitespace in strings
 *  appended with the `append` method will be replaced by a single space
 *  character, and leading and trailing space will be removed completely.
 */
class TextBuffer
{
  val sb = new StringBuilder()

  /** Appends this string to the text buffer, trimming whitespaces as needed.
   */
  def append(cs: Seq[Char]): this.type = {
    cs foreach { c =>
      if (!isSpace(c)) sb append c
      else if (sb.isEmpty || !isSpace(sb.last)) sb append ' '
    }
    this
  }

  /** Returns an empty sequence if text is only whitespace.
   *
   *  @return the text without whitespaces.
   */
  def toText: Seq[Text] = sb.toString.trim match {
    case "" => Nil
    case s  => Seq(Text(s))
  }
}
