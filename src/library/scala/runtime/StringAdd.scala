/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._

final class StringAdd(self: Any) {
  def +(other: String) = self.toString + other
  /** Formats string according to given locale and format string. Formatstrings
   *  are as for String.format (@see java.lang.String.format)
   */

  def format(locale: java.util.Locale, format: String): String =
    String.format(locale, format, Array(self.asInstanceOf[Object]))

  /** Formats string according to given format string. Formatstrings
   *  are as for String.format (@see java.lang.String.format)
   */
  def format(format: String): String =
    String.format(format, Array(self.asInstanceOf[Object]))
}

