/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */



package scala.runtime


final class StringAdd(self: Any) {

  def +(other: String) = String.valueOf(self) + other

  /** Returns string formatted according to given <code>format</code> string.
   *  Format strings are as for <code>String.format</code>
   *  (@see java.lang.String.format).
   */
  def formatted(fmtstr: String): String = fmtstr format self
}
