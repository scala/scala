/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal
package util

import java.lang.System.{lineSeparator => EOL}

/** This object provides utility methods to extract elements
 *  from Strings.
 */
trait StringOps {
  def oempty(xs: String*)        = xs filterNot (x => x == null || x == "")
  def ojoin(xs: String*): String = oempty(xs: _*) mkString " "
  def longestCommonPrefix(xs: List[String]): String = xs match {
    case Nil      => ""
    case w :: Nil => w
    case _        =>
      def lcp(ss: List[String]): String = {
        val w :: ws = ss: @unchecked
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
  def trimAllTrailingSpace(s: String): String = s.linesIterator.map(trimTrailingSpace).mkString(EOL)

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

  def splitAround(str: String, idx: Int): Option[(String, String)] = splitAt(str, idx, doDropIndex = true)

  def splitAt(str: String, idx: Int, doDropIndex: Boolean = false): Option[(String, String)] =
    if (idx == -1) None
    else Some((str take idx, str drop (if (doDropIndex) idx + 1 else idx)))

  /** Returns a string meaning "n elements".
   *  Don't try an element such as "index" with irregular plural.
   */
  def countElementsAsString(n: Int, element: String): String =
    n match {
      case 0 => s"no ${element}s"
      case 1 => s"1 ${element}"
      case _ => s"${countAsString(n)} ${element}s"
    }

  /** String conversion.
   */
  def countAsString(n: Int): String = Integer.toString(n)
}

object StringOps extends StringOps
