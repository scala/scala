/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package util

import scala.reflect.internal.Chars._

class JavaCharArrayReader(buf: IndexedSeq[Char], start: Int, /* startline: int, startcol: int, */
                      decodeUni: Boolean, error: String => Unit) extends Iterator[Char] with Cloneable {

  def this(buf: IndexedSeq[Char], decodeUni: Boolean, error: String => Unit) =
    this(buf, 0, /* 1, 1, */ decodeUni, error)

  /** the line and column position of the current character
  */
  var ch: Char = _
  var bp = start
  def cpos = bp
  var isUnicode: Boolean = _

  def hasNext = bp < buf.length

  def next(): Char = {
    val buf = this.buf.asInstanceOf[collection.mutable.WrappedArray[Char]].array
    if(!hasNext) {
      ch = SU
      return SU  // there is an endless stream of SU's at the end
    }
    ch = buf(bp)
    isUnicode = false
    bp = bp + 1
    ch match {
      case '\t' =>
      case CR =>
        if (bp < buf.length && buf(bp) == LF) {
          ch = LF
          bp += 1
        }
      case LF | FF =>
      case '\\' =>
        def evenSlashPrefix: Boolean = {
          var p = bp - 2
          while (p >= 0 && buf(p) == '\\') p -= 1
          (bp - p) % 2 == 0
        }
        def udigit: Int = {
          val d = digit2int(buf(bp), 16)
          if (d >= 0) bp += 1
          else error("error in unicode escape")
          d
        }
        if (buf(bp) == 'u' && decodeUni && evenSlashPrefix) {
          do {
            bp += 1 //; nextcol += 1
          } while (buf(bp) == 'u')
          val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
          ch = code.asInstanceOf[Char]
          isUnicode = true
        }
      case _ =>
    }
    ch
  }

  def copy: JavaCharArrayReader =
    new JavaCharArrayReader(buf, bp, /* nextcol, nextline, */ decodeUni, error)
}
