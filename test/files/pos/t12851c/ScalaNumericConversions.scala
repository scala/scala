//> using option -Werror
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
package math

/** A slightly more specific conversion trait for classes which
 *  extend ScalaNumber (which excludes value classes.)
 */
trait ScalaNumericConversions extends ScalaNumber with ScalaNumericAnyConversions {
  def underlying: Object
}

/** Conversions which present a consistent conversion interface
 *  across all the numeric types, suitable for use in value classes.
 */
trait ScalaNumericAnyConversions extends Any {
  /** @return `'''true'''` if this number has no decimal component, `'''false'''` otherwise. */
  def isWhole: Boolean
}
