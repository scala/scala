/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.text

import java.io.Writer

case object DocNil extends Document
case object DocBreak extends Document
case class DocText(txt: String) extends Document
case class DocGroup(doc: Document) extends Document
case class DocNest(indent: Int, doc: Document) extends Document
case class DocCons(hd: Document, tl: Document) extends Document

/**
 * A basic pretty-printing library, based on Lindig's strict version
 * of Wadler's adaptation of Hughes' pretty-printer.
 *
 * @author Michel Schinz
 * @version 1.0
 */
abstract class Document {
  def ::(hd: Document): Document = DocCons(hd, this)
  def ::(hd: String): Document = DocCons(DocText(hd), this)
  def :/:(hd: Document): Document = hd :: DocBreak :: this
  def :/:(hd: String): Document = hd :: DocBreak :: this

  /**
   * Format this document on `writer` and try to set line
   * breaks so that the result fits in `width` columns.
   *
   * @param width  ...
   * @param writer ...
   */
  def format(width: Int, writer: Writer) {
    type FmtState = (Int, Boolean, Document)

    def fits(w: Int, state: List[FmtState]): Boolean = state match {
      case _ if w < 0 =>
        false
      case List() =>
        true
      case (_, _, DocNil) :: z =>
        fits(w, z)
      case (i, b, DocCons(h, t)) :: z =>
        fits(w, (i,b,h) :: (i,b,t) :: z)
      case (_, _, DocText(t)) :: z =>
        fits(w - t.length(), z)
      case (i, b, DocNest(ii, d)) :: z =>
        fits(w, (i + ii, b, d) :: z)
      case (_, false, DocBreak) :: z =>
        fits(w - 1, z)
      case (_, true, DocBreak) :: z =>
        true
      case (i, _, DocGroup(d)) :: z =>
        fits(w, (i, false, d) :: z)
    }

    def spaces(n: Int) {
      var rem = n
      while (rem >= 16) { writer write "                "; rem -= 16 }
      if (rem >= 8)     { writer write "        "; rem -= 8 }
      if (rem >= 4)     { writer write "    "; rem -= 4 }
      if (rem >= 2)     { writer write "  "; rem -= 2}
      if (rem == 1)     { writer write " " }
    }

    def fmt(k: Int, state: List[FmtState]): Unit = state match {
      case List() => ()
      case (_, _, DocNil) :: z =>
        fmt(k, z)
      case (i, b, DocCons(h, t)) :: z =>
        fmt(k, (i, b, h) :: (i, b, t) :: z)
      case (i, _, DocText(t)) :: z =>
        writer write t
        fmt(k + t.length(), z)
      case (i, b, DocNest(ii, d)) :: z =>
        fmt(k, (i + ii, b, d) :: z)
      case (i, true, DocBreak) :: z =>
        writer write "\n"
        spaces(i);
        fmt(i, z)
      case (i, false, DocBreak) :: z =>
        writer write " "
        fmt(k + 1, z)
      case (i, b, DocGroup(d)) :: z =>
        val fitsFlat = fits(width - k, (i, false, d) :: z)
        fmt(k, (i, !fitsFlat, d) :: z)
    }

    fmt(0, (0, false, DocGroup(this)) :: Nil)
  }
}

object Document {
  /** The empty document */
  def empty = DocNil

  /** A break, which will either be turned into a space or a line break */
  def break = DocBreak

  /** A document consisting of some text literal */
  def text(s: String): Document = DocText(s)

  /**
   * A group, whose components will either be printed with all breaks
   * rendered as spaces, or with all breaks rendered as line breaks.
   */
  def group(d: Document): Document = DocGroup(d)

  /** A nested document, which will be indented as specified. */
  def nest(i: Int, d: Document): Document = DocNest(i, d)
}
