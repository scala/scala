/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

// $Id$


package scala.runtime


import Predef._

final class StringAdd(self: Any) {

  def +(other: String) = self.toString + other

  /** Formats string according to given <code>format</code> string.
   *  Format strings are as for <code>String.format</code> (@see
   *  http://msdn2.microsoft.com/en-us/library/system.string.format(VS.71).aspx
   *  and http://www.codeproject.com/books/0735616485.asp).
   */
  def formatted(format: String): String =
    String.Format(format, Array(self.asInstanceOf[Object]))
}
