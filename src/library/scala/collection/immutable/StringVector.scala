/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.immutable

import scala.util.matching.Regex
import scala.annotation.experimental

/** An incomplete implementation of all the methods on Vector,
 *  written as static methods which take a String as a faux "this"
 *  argument and which are all entirely String-specific, with the
 *  attendant improvements in performance, return types, etc.
 *  I looked for the fastest way to do these operations so there
 *  is some unattractiveness.
 *
 *  Martin: We should disable this because it is superseded in functionality by
 *  StringLike/StringOps. We need to clarify whether performance is good enough with the new scheme.
 *  If not, maybe we need to bring back this object in some form.
 *
 *  @author Paul Phillips
 *  @version 2.8
 */

@experimental
object StringVector
{
  private def noSuch(msg: String) = throw new NoSuchElementException(msg)

  final def foreach[T](s: String, f: Char => T): Unit = {
    val len = s.length
    var i = 0
    while (i < len) {
      f(s charAt i)
      i += 1
    }
  }

  final def filter(s: String, f: Char => Boolean): String = {
    val len = s.length
    val chars = new Array[Char](len)
    var index, i = 0
    while (i < len) {
      val c = s charAt i
      if (f(c)) {
        chars(index) = c
        index += 1
      }
      i += 1
    }
    if (index == i) s
    else new String(chars, 0, index)
  }

  final def map(s: String, f: Char => Char): String = {
    val len = s.length
    val chars = new Array[Char](len)
    var i = 0
    while (i < len) {
      chars(i) = f(s charAt i)
      i += 1
    }
    new String(chars, 0, len)
  }

  final def flatMap(s: String, f: Char => String): String = {
    val sb = new StringBuilder
    val len = s.length
    var i = 0
    while (i < len) {
      sb append f(s charAt i)
      i += 1
    }
    sb.toString()
  }

  final def take(s: String, len: Int): String =
    if (len <= 0) ""
    else if (len >= s.length) s
    else s.substring(0, len)

  final def drop(s: String, len: Int): String =
    if (len >= s.length) ""
    else if (len <= 0) s
    else s.substring(len, s.length)

  final def takeRight(s: String, len: Int): String =
    if (len <= 0) ""
    else if (len >= s.length) s
    else s.substring(s.length - len, s.length)

  final def dropRight(s: String, len: Int): String =
    if (len <= 0) s
    else if (len >= s.length) ""
    else s.substring(0, s.length - len)

  final def splitAt(s: String, index: Int): (String, String) =
    if (index <= 0) ("", s)
    else if (index >= s.length) (s, "")
    else (take(s, index), drop(s, index))

  final def head(s: String): Char =
    if (s == "") noSuch("head of empty list")
    else s charAt 0

  final def tail(s: String): String =
    if (s == "") noSuch("tail of empty list")
    else drop(s, 1)

  final def last(s: String): Char =
    if (s == "") noSuch("empty.last")
    else s charAt (s.length - 1)

  final def init(s: String): String =
    if (s == "") noSuch("empty.init")
    else dropRight(s, 1)

  final def endsWith(s: String, suffix: String) =
    takeRight(s, suffix.length) == suffix

  final def startsWith(s: String, prefix: String, offset: Int = 0) =
    take(drop(s, offset), prefix.length) == prefix

  final def takeWhile(s: String, f: Char => Boolean) = {
    val len = s.length
    var i = 0
    while (i < len && f(s charAt i)) i += 1

    take(s, i)
  }
  final def dropWhile(s: String, f: Char => Boolean) = {
    val len = s.length
    var i = 0
    while (i < len && f(s charAt i)) i += 1

    drop(s, i)
  }
  final def indexWhere(s: String, p: Char => Boolean, from: Int = 0): Int = {
    val len = s.length
    var i = from
    while (i < len) {
      if (p(s charAt i))
        return i
    }
    -1
  }
  final def lastIndexWhere(s: String, p: Char => Boolean): Int = lastIndexWhere(s, p, s.length - 1)
  final def lastIndexWhere(s: String, p: Char => Boolean, end: Int): Int = {
    var i = end
    while (i >= 0) {
      if (p(s charAt i))
        return i
      i -= 1
    }
    -1
  }

  final def reverse(s: String): String = {
    val sb = new StringBuilder
    var i = s.length - 1
    while (i >= 0) {
      sb append (s charAt i)
      i -= 1
    }
    sb.toString
  }

  final def find(s: String, p: Char => Boolean): Option[Char] = {
    val len = s.length
    var i = 0
    while (i < len) {
      val c = s charAt i
      if (p(c)) return Some(c)
    }
    None
  }

  final def count(s: String, p: Char => Boolean): Int = {
    val len = s.length
    var total, i = 0
    while (i < len)
      if (p(s charAt i))
        total += 1

    total
  }
  final def headOption(s: String): Option[Char] =
    if (s.isEmpty) None else Some(s charAt 0)
  final def lastOption(s: String): Option[Char] =
    if (s.isEmpty) None else Some(last(s))

  final def slice(s: String, from: Int, to: Int) =
    s.substring(from, to)

  final def span(s: String, p: Char => Boolean): (String, String) = {
    val len = s.length
    var i = 0
    while (p(s charAt i)) i += 1

    splitAt(s, i)
  }

  final def partition(s: String, p: Char => Boolean): (String, String) = {
    val len = s.length
    val sb1, sb2 = new StringBuilder
    var i = 0
    while (i < len) {
      val c = s charAt i
      if (p(c)) sb1 append c else sb2 append c
      i += 1
    }
    (sb1.toString, sb2.toString)
  }
}
