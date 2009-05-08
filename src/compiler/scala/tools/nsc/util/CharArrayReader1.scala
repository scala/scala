/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: NewCharArrayReader.scala 16893 2009-01-13 13:09:22Z cunei $

package scala.tools.nsc.util

import scala.tools.nsc.util.SourceFile.{LF, FF, CR, SU}

abstract class CharArrayReader1 { self =>

  val buf: Array[Char]

  def decodeUni: Boolean = true

  /** An error routine to call on bad unicode escapes \\uxxxx. */
  protected def error(offset: Int, msg: String)

  /** the last read character */
  var ch: Char = _

  /** The offset one past the last read character */
  var charOffset: Int = 0

  /** The start offset of the current line */
  var lineStartOffset: Int = 0

  /** The start offset of the line before the current one */
  var lastLineStartOffset: Int = 0

  private var lastUnicodeOffset = -1

  /** Is last character a unicode escape \\uxxxx? */
  def isUnicodeEscape = charOffset == lastUnicodeOffset

  /** Advance one character */
  final def nextChar() {
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
      if (c == '\\') potentialUnicode()
      else if (c < ' ') potentialLineEnd()
//      print("`"+ch+"'")
    }
  }

  /** Interpret \\uxxxx escapes */
  private def potentialUnicode() {
    def evenSlashPrefix: Boolean = {
      var p = charOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (charOffset - p) % 2 == 0
    }
    def udigit: Int = {
      val d = digit2int(buf(charOffset), 16)
      if (d >= 0) charOffset += 1
      else error(charOffset, "error in unicode escape")
      d
    }
    if (charOffset < buf.length && buf(charOffset) == 'u' && decodeUni && evenSlashPrefix) {
      do charOffset += 1
      while (charOffset < buf.length && buf(charOffset) == 'u')
      val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
      lastUnicodeOffset = charOffset
      ch = code.toChar
    }
  }

  /** Handle line ends, replace CR+LF by LF */
  private def potentialLineEnd() {
    if (ch == CR)
      if (charOffset < buf.length && buf(charOffset) == LF) {
        charOffset += 1
        ch = LF
      }
    if (ch == LF || ch == FF) {
      lastLineStartOffset = lineStartOffset
      lineStartOffset = charOffset
    }
  }

  /** Convert a character digit to an Int according to given base,
   *  -1 if no success */
  def digit2int(ch: Char, base: Int): Int = {
    if ('0' <= ch && ch <= '9' && ch < '0' + base)
      ch - '0'
    else if ('A' <= ch && ch < 'A' + base - 10)
      ch - 'A' + 10
    else if ('a' <= ch && ch < 'a' + base - 10)
      ch - 'a' + 10
    else
      -1
  }

  /** A new reader that takes off at the current character position */
  def lookaheadReader = new CharArrayReader1 {
    val buf = self.buf
    charOffset = self.charOffset
    ch = self.ch
    override def decodeUni = self.decodeUni
    def error(offset: Int, msg: String) = self.error(offset, msg)
  }
}
