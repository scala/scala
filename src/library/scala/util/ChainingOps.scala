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
package util

import scala.language.implicitConversions

trait ChainingSyntax {
  @inline implicit final def scalaUtilChainingOps[A](a: A): ChainingOps[A] = new ChainingOps(a)
}

/** Adds chaining methods `tap` and `pipe` to every type.
 */
final class ChainingOps[A](private val self: A) extends AnyVal {
  /** Applies `f` to the value for its side effects, and returns the original value.
    *
    * {{{
    *   scala> import scala.util.chaining._
    *
    *   scala> val xs = List(1, 2, 3).tap(ys => println("debug " + ys.toString))
    *   debug List(1, 2, 3)
    *   xs: List[Int] = List(1, 2, 3)
    * }}}
    *
    *  @param f      the function to apply to the value.
    *  @tparam U     the result type of the function `f`.
    *  @return       the original value `self`.
    */
  def tap[U](f: A => U): A = {
    f(self)
    self
  }

  /** Converts the value by applying the function `f`.
    *
    * {{{
    *   scala> import scala.util.chaining._
    *
    *   scala> val times6 = (_: Int) * 6
    *   times6: Int => Int = \$\$Lambda\$2023/975629453@17143b3b
    *
    *   scala> val i = (1 - 2 - 3).pipe(times6).pipe(scala.math.abs)
    *   i: Int = 24
    * }}}
    *
    * Note: `(1 - 2 - 3).pipe(times6)` may have a small amount of overhead at
    * runtime compared to the equivalent  `{ val temp = 1 - 2 - 3; times6(temp) }`.
    *
    *  @param f      the function to apply to the value.
    *  @tparam B     the result type of the function `f`.
    *  @return       a new value resulting from applying the given function
    *                `f` to this value.
    */
  def pipe[B](f: A => B): B = f(self)
}
