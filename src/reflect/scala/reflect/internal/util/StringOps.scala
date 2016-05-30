/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package reflect
package internal
package util

import scala.compat.Platform.EOL

/** This object provides utility methods to extract elements
 *  from Strings.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
trait StringOps {
  def oempty(xs: String*)        = xs filterNot (x => x == null || x == "")
  def ojoin(xs: String*): String = oempty(xs: _*) mkString " "
  def longestCommonPrefix(xs: List[String]): String = xs match {
    case Nil      => ""
    case w :: Nil => w
    case _        =>
      def lcp(ss: List[String]): String = {
        val w :: ws = ss
        if (w == "") ""
        else if (ws exists (s => s == "" || (s charAt 0) != (w charAt 0))) ""
        else w.substring(0, 1) + lcp(ss map (_ substring 1))
      }
      lcp(xs)
  }
  /** Like String#trim, but trailing whitespace only.
   */
  def trimTrailingSpace(s: String): String = {
    var end = s.length
    while (end > 0 && s.charAt(end - 1).isWhitespace)
      end -= 1

    if (end == s.length) s
    else s.substring(0, end)
  }
  /** Breaks the string into lines and strips each line before reassembling. */
  def trimAllTrailingSpace(s: String): String = s.lines map trimTrailingSpace mkString EOL

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

  def splitWhere(str: String, f: Char => Boolean, doDropIndex: Boolean = false): Option[(String, String)] =
    splitAt(str, str indexWhere f, doDropIndex)

  def splitAt(str: String, idx: Int, doDropIndex: Boolean = false): Option[(String, String)] =
    if (idx == -1) None
    else Some((str take idx, str drop (if (doDropIndex) idx + 1 else idx)))

  /** Returns a string meaning "n elements".
   */
  def countElementsAsString(n: Int, elements: String): String =
    n match {
      case 0 => s"no ${elements}s"
      case 1 => "one "   + elements
      case 2 => "two "   + elements + "s"
      case 3 => "three " + elements + "s"
      case 4 => "four "  + elements + "s"
      case _ => s"$n ${elements}s"
    }

  /** Turns a count into a friendly English description if n<=4.
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
