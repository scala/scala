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


final class RichBoolean(private val self: Boolean) extends AnyVal {

  /** Result of comparing `this` with operand `that`.
   *
   * Returns `x` where:
   *
   *  - `x < 0` when `this < that`
   *  - `x == 0` when `this == that`
   *  - `x > 0` when  `this > that`
   *
   * For the purposes of this method, `false` is considered less than `true`.
   */
  def compare(y: Boolean): Int = java.lang.Boolean.compare(self, y)

  /** Returns true if `this` is less than `that` */
  def <(that: Boolean): Boolean = !self && that

  /** Returns true if `this` is greater than `that`. */
  def >(that: Boolean): Boolean = self && !that

  /** Returns true if `this` is less than or equal to `that`. */
  def <=(that: Boolean): Boolean = !self || that

  /** Returns true if `this` is greater than or equal to `that`. */
  def >=(that: Boolean): Boolean = self || !that
}
