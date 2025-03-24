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

package scala.tools.nsc
package interactive

@deprecated("Use scala.reflect.internal.Positions", "2.11.0")
trait RangePositions extends scala.reflect.internal.Positions with ast.Trees with ast.Positions {
  self: scala.tools.nsc.Global =>

  override val useOffsetPositions = false
}
