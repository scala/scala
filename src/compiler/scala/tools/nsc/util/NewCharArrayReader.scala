/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

import scala.tools.nsc.util.SourceFile.{LF, FF, CR, SU}

class NewCharArrayReader(val buf: RandomAccessSeq[Char], // should not change
                       decodeUni: Boolean, error: (Int,String) => Unit) extends Iterator[Char] {
  private var idx : Int = 0
  private var isUnicode0 = false
  def isUnicode = isUnicode0

  private val bufLength = buf.length
  def seek(offset : Int) = {
    assert(offset <= bufLength)
    idx = offset
  }
  def offset = idx
  def withOffset = new Iterator[(Int,Char)] {
    def hasNext = NewCharArrayReader.this.hasNext
    def next = (offset, NewCharArrayReader.this.next)
  }
  override def hasNext = { // could be padded
    if (idx == bufLength - 1) buf(idx) != SU
    else idx < bufLength
  }
  override def next : Char = {
    isUnicode0 = false
    if (!hasNext) return SU
    var ch = buf(idx)
    idx = idx + 1
    ch match {
    case CR if (idx + 1 < length && buf(idx + 1) == LF) =>
      idx += 1; ch = LF
    case LF | FF =>
    case '\\' =>
      def evenSlashPrefix: Boolean = {
        var p = idx - 2
        while (p >= 0 && buf(p) == '\\') p = p - 1;
        (idx - p) % 2 == 0
      }
      def udigit: Int = {
        val d = digit2int(buf(idx), 16)
        if (d >= 0) { idx = idx + 1 }
        else if (error != null) error(idx, "error in unicode escape");
        d
      }
      if (idx < bufLength && buf(idx) == 'u' && decodeUni && evenSlashPrefix) {
        do {
          idx = idx + 1; // nextcol = nextcol + 1;
        } while (idx < bufLength && buf(idx) == 'u');
        val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
        isUnicode0 = true
        ch = code.asInstanceOf[Char]
      }
    case _ =>
    }
    ch
  }
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
}
