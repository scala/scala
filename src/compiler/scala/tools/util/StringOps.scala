/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools
package util

/** This object provides utility methods to extract elements
 *  from Strings.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object StringOps {
  /**
   * Split command line parameters by space, properly process quoted parameter
   */
  def splitParams(line: String, errorFn: String => Unit): List[String] = {
    def parse(from: Int, i: Int, args: List[String]): List[String] = {
      if (i < line.length) {
        line.charAt(i) match {
          case ' ' =>
            val args1 = fetchArg(from, i) :: args
            val j = skipS(i + 1)
            if (j >= 0) {
              parse(j, j, args1)
            } else args1
          case '"' =>
            val j = skipTillQuote(i + 1)
            if (j > 0) {
              parse(from, j + 1, args)
            } else {
              errorFn("Parameters '" + line + "' with unmatched quote at " + i + ".")
              Nil
            }
          case _ => parse(from, i + 1, args)
        }
      } else { // done
        if (i > from) {
          fetchArg(from, i) :: args
        } else args
      }
    }

    def fetchArg(from: Int, until: Int) = {
      if (line.charAt(from) == '"') {
        line.substring(from + 1, until - 1)
      } else {
        line.substring(from, until)
      }
    }

    def skipTillQuote(i: Int): Int = {
      if (i < line.length) {
        line.charAt(i) match {
          case '"' => i
          case _ => skipTillQuote(i + 1)
        }
      } else -1
    }

    def skipS(i: Int): Int = {
      if (i < line.length) {
        line.charAt(i) match {
          case ' ' => skipS(i + 1)
          case _ => i
        }
      } else -1
    }

    // begin split
    val j = skipS(0)
    if (j >= 0) {
      parse(j, j, Nil).reverse
    } else Nil
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
    else Some(str take idx, str drop (if (doDropIndex) idx + 1 else idx))
}
