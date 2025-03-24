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
package math

import scala.language.implicitConversions

trait Integral[T] extends Numeric[T] {
  def quot(x: T, y: T): T
  def rem(x: T, y: T): T

  class IntegralOps(lhs: T) extends NumericOps(lhs) {
    def /(rhs: T) = quot(lhs, rhs)
    def %(rhs: T) = rem(lhs, rhs)
    def /%(rhs: T) = (quot(lhs, rhs), rem(lhs, rhs))
  }
  override implicit def mkNumericOps(lhs: T): IntegralOps = new IntegralOps(lhs)
}

object Integral {
  @inline def apply[T](implicit int: Integral[T]): Integral[T] = int

  trait ExtraImplicits {
    /** The regrettable design of Numeric/Integral/Fractional has them all
     *  bumping into one another when searching for this implicit, so they
     *  are exiled into their own companions.
     */
    implicit def infixIntegralOps[T](x: T)(implicit num: Integral[T]): Integral[T]#IntegralOps = new num.IntegralOps(x)
  }
  object Implicits extends ExtraImplicits
}
