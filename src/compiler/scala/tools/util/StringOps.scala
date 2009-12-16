/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.util

/** This objects provides methods to extract elements from
 *  a string according to some defined character separator.
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
}
