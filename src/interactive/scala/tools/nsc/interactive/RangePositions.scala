/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package interactive

@deprecated("Use scala.reflect.internal.Positions", "2.11.0")
trait RangePositions extends scala.reflect.internal.Positions with ast.Trees with ast.Positions {
  self: scala.tools.nsc.Global =>

  override val useOffsetPositions = false
}
