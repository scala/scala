package scala.tools.nsc
package scratchpad

import java.io.Writer
import scala.reflect.internal.util.SourceFile

import reflect.internal.Chars._

object SourceInserter {
  def stripRight(cs: Array[Char]): Array[Char] = {
    val lines =
      new String(cs) split "\n"
    def leftPart(str: String) =
      (str split """//>|//\|""").head
    def isContinuation(str: String) =
      ((str contains "//>") || (str contains "//|")) && (leftPart(str) forall isWhitespace)
    def stripTrailingWS(str: String) =
      str take (str lastIndexWhere (!isWhitespace(_))) + 1
    val prefixes =
      lines filterNot isContinuation map leftPart map stripTrailingWS
    (prefixes mkString "\n").toArray
  }
}
class SourceInserter(contents: Array[Char], start: Int = 0, tabInc: Int = 8) extends Writer {

  private var buf = contents
  private var offset = start
  private var hilen = contents.length

  def length = offset + hilen

  private def currentColumn: Int = {
    var i = offset
    while (i > 0 && !isLineBreakChar(buf(i - 1))) i -= 1
    var col = 0
    while (i < offset) {
      col = if (buf(i) == '\t') (col + tabInc) / tabInc * tabInc else col + 1
      i += 1
    }
    col
  }

  private var col = currentColumn

  def column = synchronized { col }

  private def addCapacity(n: Int) = {
    val newlength = length + n
    while (newlength > buf.length) {
      val buf1 = Array.ofDim[Char](buf.length * 2)
      Array.copy(buf, 0, buf1, 0, offset)
      Array.copy(buf, buf.length - hilen, buf1, buf1.length - hilen, hilen)
      buf = buf1
    }
  }

  private def insertChar(ch: Char) = {
//  Console.err.print("["+ch+"]")
    buf(offset) = ch
    offset += 1
    ch match {
      case LF => col = 0
      case '\t' => col = (col + tabInc) / tabInc * tabInc
      case _ => col += 1
    }
  }

  override def write(ch: Int) = synchronized {
    addCapacity(1)
    insertChar(ch.toChar)
  }

  override def write(chs: Array[Char], off: Int, len: Int) = synchronized {
    addCapacity(len)
    for (i <- off until off + len) insertChar(chs(i))
  }

  override def close() {
  }

  override def flush() {
    // signal buffer change
  }

  def currentContents = synchronized {
    if (length == buf.length) buf
    else {
      val res = Array.ofDim[Char](length)
      Array.copy(buf, 0, res, 0, offset)
      Array.copy(buf, buf.length - hilen, res, offset, hilen)
      res
    }
  }

  def backspace() = synchronized {
    offset -= 1
    if (offset > 0 && buf(offset) == LF && buf(offset - 1) == CR) offset -=1
  }

  def currentChar = synchronized {
    buf(buf.length - hilen)
  }

  def skip(len: Int) = synchronized {
    for (i <- 0 until len) {
      val ch = currentChar
      hilen -= 1
      insertChar(ch)
    }
  }
}

