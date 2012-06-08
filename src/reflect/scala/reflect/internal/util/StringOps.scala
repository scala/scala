/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.reflect.internal.util

/** This object provides utility methods to extract elements
 *  from Strings.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait StringOps {
  def onull(s: String)                            = if (s == null) "" else s
  def oempty(xs: String*)                         = xs filterNot (x => x == null || x == "")
  def ojoin(xs: String*): String                  = oempty(xs: _*) mkString " "
  def ojoin(xs: Seq[String], sep: String): String = oempty(xs: _*) mkString sep
  def ojoinOr(xs: Seq[String], sep: String, orElse: String) = {
    val ys = oempty(xs: _*)
    if (ys.isEmpty) orElse else ys mkString sep
  }
  def trimTrailingSpace(s: String) = {
    if (s.length == 0 || !s.charAt(s.length - 1).isWhitespace) s
    else {
      var idx = s.length - 1
      while (idx >= 0 && s.charAt(idx).isWhitespace)
        idx -= 1

      s.substring(0, idx + 1)
    }
  }

  def decompose(str: String, sep: Char): List[String] = {
    def ws(start: Int): List[String] =
      if (start == str.length) List()
      else if (str.charAt(start) == sep) ws(start + 1)
      else {
        val end = str.indexOf(sep, start)
        if (end < 0) List(str.substring(start))
        else str.substring(start, end) :: ws(end + 1)
      }
    ws(0)
  }

  def words(str: String): List[String] = decompose(str, ' ')

  def stripPrefixOpt(str: String, prefix: String): Option[String] =
    if (str startsWith prefix) Some(str drop prefix.length)
    else None

  def stripSuffixOpt(str: String, suffix: String): Option[String] =
    if (str endsWith suffix) Some(str dropRight suffix.length)
    else None

  def splitWhere(str: String, f: Char => Boolean, doDropIndex: Boolean = false): Option[(String, String)] =
    splitAt(str, str indexWhere f, doDropIndex)

  def splitAt(str: String, idx: Int, doDropIndex: Boolean = false): Option[(String, String)] =
    if (idx == -1) None
    else Some((str take idx, str drop (if (doDropIndex) idx + 1 else idx)))

  /** Returns a string meaning "n elements".
   *
   *  @param n        ...
   *  @param elements ...
   *  @return         ...
   */
  def countElementsAsString(n: Int, elements: String): String =
    n match {
      case 0 => "no "    + elements + "s"
      case 1 => "one "   + elements
      case 2 => "two "   + elements + "s"
      case 3 => "three " + elements + "s"
      case 4 => "four "  + elements + "s"
      case _ => "" + n + " " + elements + "s"
    }

  /** Turns a count into a friendly English description if n<=4.
   *
   *  @param n        ...
   *  @return         ...
   */
  def countAsString(n: Int): String =
    n match {
      case 0 => "none"
      case 1 => "one"
      case 2 => "two"
      case 3 => "three"
      case 4 => "four"
      case _ => "" + n
    }
}

object StringOps extends StringOps { }
