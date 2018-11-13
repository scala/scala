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
package util

trait ChainingSyntax {
  implicit final def scalaUtilChainingOps[A](a: A): ChainingOps[A] = new ChainingOps(a)
}

/** Adds chaining methods `tap`, `pipe` (aliased as `|>`) to every type.
 *
 * Note: these methods may have a small amount of overhead compared to their expanded forms.
 */
final class ChainingOps[A](private val self: A) extends AnyVal {
  /** Applies `f` to the value for its side effects, and returns the original value.
   *
   *    {{{
   *    val xs = List(1, 2, 3)
   *               .tap(ys => println("debug " + ys.toString))
   *    // xs == List(1, 2, 3)
   *    }}}
   *
   *  @param f      the function to apply to the value.
   *  @tparam U     the result type of the function `f`.
   *  @return       the original value `self`.
   */
  def tap[U](f: A => U): A = {
    f(self)
    self
  }


  /** Applies function `f` to the value.
   *
   *    {{{
   *    val i = (1 - 2).pipe(_ + 3).pipe(scala.math.abs)
   *    // i == 2
   *    }}}
   *
   * It is equivalent to:
   *
   *    {{{
   *    val i = scala.math.abs((1 - 2) + 3)
   *    // i == 2
   *    }}}
   *
   *  @param f      the function to apply to the value.
   *  @tparam B     the result type of the function `f`.
   *  @return       the result of applying `f` to the value.
   */
  def pipe[B](f: A => B): B = f(self)


  /** Applies function `f` to the value.
   *
   *    {{{
   *    val i = (1 - 2) |> (_ + 3) |> scala.math.abs
   *    // i == 2
   *    }}}
   *
   * It is equivalent to:
   *
   *    {{{
   *    val i = scala.math.abs((1 - 2) + 3)
   *    // i == 2
   *    }}}
   *
   * The `|>` syntax is inspired by that of F#, Bash, OCaml, ReasonML, Elixir and PowerShell.
   * It is an alias of [[pipe]].
   *
   *  @param f      the function to apply to the value.
   *  @tparam B     the result type of the function `f`.
   *  @return       the result of applying `f` to the value.
   */
  def |>[B](f: A => B): B = pipe(f)
}
