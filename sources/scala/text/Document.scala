/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.text;

import java.io.PrintWriter;
import java.io.Writer;

case object DocNil extends Document;
case object DocBreak extends Document;
case class DocText(txt: String) extends Document;
case class DocGroup(doc: Document) extends Document;
case class DocNest(indent: Int, doc: Document) extends Document;
case class DocCons(hd: Document, tl: Document) extends Document;

/**
 * A basic pretty-printing library, based on Lindig's strict version
 * of Wadler's adaptation of Hughes' pretty-printer.
 *
 * @version 1.0
 * @author Michel Schinz
 */

abstract class Document {
  def ::(hd: Document): Document = DocCons(hd, this);
  def ::(hd: String): Document = DocCons(DocText(hd), this);
  def :/:(hd: Document): Document = hd :: DocBreak :: this;
  def :/:(hd: String): Document = hd :: DocBreak :: this;

  /**
   * Format this document on WRITER and try to set line breaks so that
   * the result fits in WIDTH columns.
   */
  def format(width: Int, writer: Writer): Unit = {
    type FmtState = Triple[Int,Boolean,Document];

    def fits(w: Int, state: List[FmtState]): boolean = state match {
      case _ if w < 0 =>
        false
      case List() =>
        true
      case Triple(_, _, DocNil) :: z =>
        fits(w, z)
      case Triple(i, b, DocCons(h, t)) :: z =>
        fits(w, Triple(i,b,h) :: Triple(i,b,t) :: z)
      case Triple(_, _, DocText(t)) :: z =>
        fits(w - t.length(), z)
      case Triple(i, b, DocNest(ii, d)) :: z =>
        fits(w, Triple(i + ii, b, d) :: z)
      case Triple(_, false, DocBreak) :: z =>
        fits(w - 1, z)
      case Triple(_, true, DocBreak) :: z =>
        true
      case Triple(i, _, DocGroup(d)) :: z =>
        fits(w, Triple(i, false, d) :: z)
    }

    def spaces(n: Int): Unit = {
      var rem = n;
      while (rem >= 16) { writer write "                "; rem = rem - 16 };
      if (rem >= 8)     { writer write "        "; rem = rem - 8 };
      if (rem >= 4)     { writer write "    "; rem = rem - 4 };
      if (rem >= 2)     { writer write "  "; rem = rem - 2};
      if (rem == 1)     { writer write " " };
    }

    def fmt(k: Int, state: List[FmtState]): Unit = state match {
      case List() => ()
      case Triple(_, _, DocNil) :: z =>
        fmt(k, z)
      case Triple(i, b, DocCons(h, t)) :: z =>
        fmt(k, Triple(i, b, h) :: Triple(i, b, t) :: z)
      case Triple(i, _, DocText(t)) :: z =>
        writer write t;
        fmt(k + t.length(), z)
      case Triple(i, b, DocNest(ii, d)) :: z =>
        fmt(k, Triple(i + ii, b, d) :: z)
      case Triple(i, true, DocBreak) :: z =>
        writer write "\n";
        spaces(i);
        fmt(i, z)
      case Triple(i, false, DocBreak) :: z =>
        writer write " ";
        fmt(k + 1, z)
      case Triple(i, b, DocGroup(d)) :: z =>
        val fitsFlat = fits(width - k, Triple(i, false, d) :: z);
        fmt(k, Triple(i, !fitsFlat, d) :: z)
    }

    fmt(0, Triple(0, false, DocGroup(this)) :: Nil);
  }
}

object Document {
  /** The empty document */
  def empty = DocNil;

  /** A break, which will either be turned into a space or a line break */
  def break = DocBreak;

  /** A document consisting of some text literal */
  def text(s: String): Document = DocText(s);

  /**
   * A group, whose components will either be printed with all breaks
   * rendered as spaces, or with all breaks rendered as line breaks.
   */
  def group(d: Document): Document = DocGroup(d);

  /** A nested document, which will be indented as specified. */
  def nest(i: Int, d: Document): Document = DocNest(i, d);
}
