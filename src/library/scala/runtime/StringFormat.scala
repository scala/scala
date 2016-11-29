/*                                                                      *\
**     ________ ___   __   ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ |_|                                         **
**                                                                      **
\*                                                                      */

package scala
package runtime


/** A wrapper class that adds a `formatted` operation to any value
 */
@deprecated("use Predef.StringFormat", "2.11.0")
final class StringFormat(val self: Any) extends AnyVal {
  /** Returns string formatted according to given `format` string.
   *  Format strings are as for `String.format`
   *  (@see java.lang.String.format).
   */
  @inline def formatted(fmtstr: String): String = fmtstr format self
}
