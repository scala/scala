/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

import scala.tools.nsc.util.SourceFile.{LF, FF, CR, SU}

class CharArrayReader(buf: Array[char], start: int, /* startline: int, startcol: int, */
                      decodeUni: boolean, error: String => unit) {

  def this(buf: Array[char], decodeUni: boolean, error: String => unit) =
    this(buf, 0, /* 1, 1, */ decodeUni, error)

  /** layout constant
   */
  val tabinc = 8

  /** the line and column position of the current character
  */
  var ch: char = _
  var bp = start
  //private var cline: int = _
  //private var ccol: int = _
  def cpos = bp
  var isUnicode: boolean = _
  var lastLineStartPos: int = 0
  var lineStartPos: int = 0
  var lastBlankLinePos: int = 0

  private var onlyBlankChars = false
  //private var nextline = startline
  //private var nextcol = startcol

  private def markNewLine() {
    lastLineStartPos = lineStartPos
    if (onlyBlankChars) lastBlankLinePos = lineStartPos
    lineStartPos = bp
    onlyBlankChars = true
    //nextline = nextline + 1;
    //nextcol = 1;
  }

  def hasNext: boolean = bp < buf.length

  def last: char = if(bp > start + 2) buf(bp - 2) else ' ' // XML literals

  def next: unit = {
    //cline = nextline
    //ccol = nextcol
    if(!hasNext) return SU  // there is an endless stream of SU's at the end
    ch = buf(bp)
    isUnicode = false
    bp = bp + 1
    ch match {
      case '\t' =>
        // nextcol = ((nextcol - 1) / tabinc * tabinc) + tabinc + 1;
      case CR =>
        if (buf(bp) == LF) {
          ch = LF
          bp = bp + 1
        }
        markNewLine()
      case LF | FF =>
        markNewLine()
      case '\\' =>
        def evenSlashPrefix: boolean = {
          var p = bp - 2
          while (p >= 0 && buf(p) == '\\') p = p - 1;
          (bp - p) % 2 == 0
        }
        def udigit: int = {
          val d = digit2int(buf(bp), 16)
          if (d >= 0) { bp = bp + 1; /* nextcol = nextcol + 1 */ }
          else error("error in unicode escape");
          d
        }
        // nextcol = nextcol + 1
        if (buf(bp) == 'u' && decodeUni && evenSlashPrefix) {
          do {
            bp = bp + 1; // nextcol = nextcol + 1;
          } while (buf(bp) == 'u');
          val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
          ch = code.asInstanceOf[char]
          isUnicode = true
        }
      case _ =>
        if (ch > ' ') onlyBlankChars = false
        // nextcol = nextcol + 1
    }
  }

  def copy: CharArrayReader =
    new CharArrayReader(buf, bp, /* nextcol, nextline, */ decodeUni, error)

  def digit2int(ch: char, base: int): int = {
    if ('0' <= ch && ch <= '9' && ch < '0' + base)
      ch - '0'
    else if ('A' <= ch && ch < 'A' + base - 10)
      ch - 'A' + 10
    else if ('a' <= ch && ch < 'a' + base - 10)
      ch - 'a' + 10
    else
      -1
  }
}
