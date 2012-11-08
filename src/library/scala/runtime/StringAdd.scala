/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

package scala.runtime

/** A wrapper class that adds string concatenation `+` to any value */
final class StringAdd(val self: Any) extends AnyVal {
  def +(other: String) = String.valueOf(self) + other
}
