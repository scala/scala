/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml


object TextBuffer {
  def fromString(str: String): TextBuffer = new TextBuffer().append(str)
}

/** The class <code>TextBuffer</code> is for creating text nodes without
 *  surplus whitespace. All occurrences of one or more whitespace in strings
 *  appended with the <code>append</code> method will be replaced by a single
 *  space character, and leading and trailing space will be removed completely.
 */
class TextBuffer {

  val sb = new StringBuilder()
  var ws = true

  def appendSpace         = if(!ws) { ws = true;  sb.append(' ') } else {}
  def appendChar(c: Char) =         { ws = false; sb.append( c ) }

  /** Appends this string to the text buffer, trimming whitespaces as needed.
   *
   *  @param  cs ...
   *  @return ...
   */
  def append(cs: Seq[Char]): TextBuffer = {
    for (c <- cs)
      if (Utility.isSpace(c)) appendSpace else appendChar(c)
    this
  }

  /** Returns an empty sequence if text is only whitespace.
   *
   *  @return the text without whitespaces.
   */
  def toText: Seq[Text] = {
    var len = sb.length /* invariant */
    if (len == 0) return Nil

    if (Utility.isSpace(sb.charAt(len - 1))) {
      len -= 1
      sb.length = len
    }
    if (len == 0) return Nil

    List(Text(sb.toString()))
  }

}
