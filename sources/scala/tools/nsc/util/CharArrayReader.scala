/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.util;

import scala.tools.util.SourceFile.{LF, FF, CR, SU}

class CharArrayReader(buf: Array[char], start: int, startline: int, startcol: int,
                      decodeUni: boolean, error: String => unit) {

  def this(buf: Array[char], decodeUni: boolean, error: String => unit) =
    this(buf, 0, 1, 1, decodeUni, error);

  /** layout constant
   */
  val tabinc = 8;

  /** the line and column position of the current character
  */
  var ch: char = _;
  var bp = start;
  var cline: int = _;
  var ccol: int = _;
  private var nextline = startline;
  private var nextcol = startcol;

  def hasNext: boolean = bp < buf.length;

  def next: unit = {
    cline = nextline;
    ccol = nextcol;
    ch = buf(bp);
    bp = bp + 1;
    ch match {
      case '\t' =>
        nextcol = ((nextcol - 1) / tabinc * tabinc) + tabinc + 1;
      case CR =>
        nextline = nextline + 1;
        nextcol = 1;
        if (buf(bp) == LF) {
          ch = LF;
          bp = bp + 1
        }
      case LF | FF =>
        nextline = nextline + 1;
        nextcol = 1
      case '\\' =>
        def evenSlashPrefix: boolean = {
          var p = bp - 2;
          while (p >= 0 && buf(p) == '\\') p = p - 1;
          (bp - p) % 2 == 0
        }
        def udigit: int = {
          val d = digit2int(buf(bp), 16);
          if (d >= 0) { bp = bp + 1; nextcol = nextcol + 1 }
          else error("error in unicode escape");
          d
        }
        nextcol = nextcol + 1;
        if (buf(bp) == 'u' && decodeUni && evenSlashPrefix) {
          do {
            bp = bp + 1; nextcol = nextcol + 1;
          } while (buf(bp) == 'u');
          val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit;
          ch = code.asInstanceOf[char]
        }
      case _ =>
        nextcol = nextcol + 1
    }
  }

  def copy: CharArrayReader =
    new CharArrayReader(buf, bp, nextcol, nextline, decodeUni, error);

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
