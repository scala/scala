/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime


final class RichBoolean(val self: Boolean) extends AnyVal with OrderedProxy[Boolean] {
  protected def ord: scala.math.Ordering.Boolean.type = scala.math.Ordering.Boolean
}
