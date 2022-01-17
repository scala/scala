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

// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


object Function1 {

  implicit final class UnliftOps[A, B] private[Function1](private val f: A => Option[B]) extends AnyVal {
    /** Converts an optional function to a partial function.
      *
      * @example Unlike [[Function.unlift]], this [[UnliftOps.unlift]] method can be used in extractors.
      *          {{{
      *          val of: Int => Option[String] = { i =>
      *            if (i == 2) {
      *              Some("matched by an optional function")
      *            } else {
      *              None
      *            }
      *          }
      *
      *          util.Random.nextInt(4) match {
      *            case of.unlift(m) => // Convert an optional function to a pattern
      *              println(m)
      *            case _ =>
      *              println("Not matched")
      *          }
      *          }}}
      */
    def unlift: PartialFunction[A, B] = Function.unlift(f)
  }

}

/** A function of 1 parameter.
 *  
 *  In the following example, the definition of `succ` is
 *  shorthand, conceptually, for the anonymous class definition
 *  `anonfun1`, although the implementation details of how the
 *  function value is constructed may differ:
 *
 *  {{{
 *  object Main extends App {
 *    val succ = (x: Int) => x + 1
 *    val anonfun1 = new Function1[Int, Int] {
 *      def apply(x: Int): Int = x + 1
 *    }
 *    assert(succ(0) == anonfun1(0))
 * }
 *  }}}
 *
 *  Note that the difference between `Function1` and [[scala.PartialFunction]]
 *  is that the latter can specify inputs which it will not handle.
 */
@annotation.implicitNotFound(msg = "No implicit view available from ${T1} => ${R}.")
trait Function1[@specialized(Specializable.Arg) -T1, @specialized(Specializable.Return) +R] extends AnyRef { self =>
  /** Apply the body of this function to the argument.
   *  @return   the result of function application.
   */
  def apply(v1: T1): R

  /** Composes two instances of Function1 in a new Function1, with this function applied last.
   *
   *  @tparam   A   the type to which function `g` can be applied
   *  @param    g   a function A => T1
   *  @return       a new function `f` such that `f(x) == apply(g(x))`
   */
  @annotation.unspecialized def compose[A](g: A => T1): A => R = { x => apply(g(x)) }

  /** Composes two instances of Function1 in a new Function1, with this function applied first.
   *
   *  @tparam   A   the result type of function `g`
   *  @param    g   a function R => A
   *  @return       a new function `f` such that `f(x) == g(apply(x))`
   */
  @annotation.unspecialized def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }

  override def toString(): String = "<function1>"
}
