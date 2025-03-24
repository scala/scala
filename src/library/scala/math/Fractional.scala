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

trait Fractional[T] extends Numeric[T] {
  def div(x: T, y: T): T

  class FractionalOps(lhs: T) extends NumericOps(lhs) {
    def /(rhs: T) = div(lhs, rhs)
  }
  override implicit def mkNumericOps(lhs: T): FractionalOps =
    new FractionalOps(lhs)
}

object Fractional {
  @inline def apply[T](implicit frac: Fractional[T]): Fractional[T] = frac

  trait ExtraImplicits {
    implicit def infixFractionalOps[T](x: T)(implicit num: Fractional[T]): Fractional[T]#FractionalOps = new num.FractionalOps(x)
  }
  object Implicits extends ExtraImplicits
}
