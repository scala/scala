/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.tools
package util

/** This object provides utility methods to extract elements
 *  from Strings.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object StringOps {
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
    else Some(str take idx, str drop (if (doDropIndex) idx + 1 else idx))
}
