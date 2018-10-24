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


/** A wrapper class that adds string concatenation `+` to any value */
@deprecated("use Predef.StringAdd", "2.11.0")
final class StringAdd(val self: Any) extends AnyVal {
  def +(other: String) = String.valueOf(self) + other
}
