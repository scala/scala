/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
