/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

import scala.reflect.internal.Chars._

trait CharArrayReaderData {
  /** the last read character */
  var ch: Char = _

  /** The offset one past the last read character */
  var charOffset: Int = 0

  /** The start offset of the current line */
  var lineStartOffset: Int = 0

  /** The start offset of the line before the current one */
  var lastLineStartOffset: Int = 0

  protected var lastUnicodeOffset = -1

  def copyFrom(cd: CharArrayReaderData): this.type = {
    this.ch = cd.ch
    this.charOffset = cd.charOffset
    this.lineStartOffset = cd.lineStartOffset
    this.lastLineStartOffset = cd.lastLineStartOffset
    this.lastUnicodeOffset = cd.lastUnicodeOffset
    this
  }
}

abstract class CharArrayReader extends CharArrayReaderData { self =>

  val buf: Array[Char]

  def decodeUni: Boolean = true

  /** An error routine to call on bad unicode escapes \\uxxxx. */
  protected def error(offset: Int, msg: String): Unit

  /** Is last character a unicode escape \\uxxxx? */
  def isUnicodeEscape = charOffset == lastUnicodeOffset

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
      if (c == '\\') potentialUnicode()
      if (ch < ' ') {
        skipCR()
        potentialLineEnd()
      }
    }
  }

  /** Advance one character, leaving CR;LF pairs intact.
   *  This is for use in multi-line strings, so there are no
   *  "potential line ends" here.
   */
  final def nextRawChar() {
    if (charOffset >= buf.length) {
      ch = SU
    } else {
      val c = buf(charOffset)
      ch = c
      charOffset += 1
      if (c == '\\') potentialUnicode()
    }
  }

  /** Interpret \\uxxxx escapes */
  private def potentialUnicode() = {
    def evenSlashPrefix: Boolean = {
      var p = charOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (charOffset - p) % 2 == 0
    }
    def udigit: Int = {
      if (charOffset >= buf.length) {
        // Since the positioning code is very insistent about throwing exceptions,
        // we have to decrement the position so our error message can be seen, since
        // we are one past EOF.  This happens with e.g. val x = \ u 1 <EOF>
        error(charOffset - 1, "incomplete unicode escape")
        SU
      }
      else {
        val d = digit2int(buf(charOffset), 16)
        if (d >= 0) charOffset += 1
        else error(charOffset, "error in unicode escape")
        d
      }
    }
    if (charOffset < buf.length && buf(charOffset) == 'u' && decodeUni && evenSlashPrefix) {
      do charOffset += 1
      while (charOffset < buf.length && buf(charOffset) == 'u')
      val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
      lastUnicodeOffset = charOffset
      ch = code.toChar
    }
  }

  /** replace CR;LF by LF */
  private def skipCR() =
    if (ch == CR && charOffset < buf.length)
      buf(charOffset) match {
        case LF =>
          charOffset += 1
          ch = LF
        case '\\' =>
          if (lookaheadReader.getu == LF)
            potentialUnicode()
        case _ =>
      }

  /** Handle line ends */
  private def potentialLineEnd() {
    if (ch == LF || ch == FF) {
      lastLineStartOffset = lineStartOffset
      lineStartOffset = charOffset
    }
  }

  /** A new reader that takes off at the current character position */
  def lookaheadReader = new CharArrayLookaheadReader

  class CharArrayLookaheadReader extends CharArrayReader {
    val buf = self.buf
    charOffset = self.charOffset
    ch = self.ch
    override def decodeUni = self.decodeUni
    def error(offset: Int, msg: String) = self.error(offset, msg)
    /** A mystery why CharArrayReader.nextChar() returns Unit */
    def getc() = { nextChar() ; ch }
    def getu() = { require(buf(charOffset) == '\\') ; ch = '\\' ; charOffset += 1 ; potentialUnicode() ; ch }
  }
}
