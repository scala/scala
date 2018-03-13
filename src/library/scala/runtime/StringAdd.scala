/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

package scala
package runtime


/** A wrapper class that adds string concatenation `+` to any value */
// TODO: remove in 2.13.0-M5 (once M4 is starr and this class is no longer required to launch the compiler)
@deprecated("use Predef.StringAdd", "2.11.0")
final class StringAdd(private val self: Any) extends AnyVal {
  def +(other: String) = String.valueOf(self) + other
}
