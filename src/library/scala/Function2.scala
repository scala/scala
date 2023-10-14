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


/** A function of 2 parameters.
 *  
 *  In the following example, the definition of `max` is
 *  shorthand, conceptually, for the anonymous class definition
 *  `anonfun2`, although the implementation details of how the
 *  function value is constructed may differ:
 *
 *  {{{
 *  object Main extends App {
 *    val max = (x: Int, y: Int) => if (x < y) y else x
 *
 *    val anonfun2 = new Function2[Int, Int, Int] {
 *      def apply(x: Int, y: Int): Int = if (x < y) y else x
 *    }
 *    assert(max(0, 1) == anonfun2(0, 1))
 * }
 *  }}}
 */
trait Function2[@specialized(Specializable.Args) -T1, @specialized(Specializable.Args) -T2, @specialized(Specializable.Return) +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2) == apply(x1, x2)`
   */
  @annotation.unspecialized def curried: T1 => T2 => R = {
    (x1: T1) => (x2: T2) => apply(x1, x2)
  }
  /** Creates a tupled version of this function: instead of 2 arguments,
   *  it accepts a single [[scala.Tuple2]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2)) == f(Tuple2(x1, x2)) == apply(x1, x2)`
   */

  @annotation.unspecialized def tupled: ((T1, T2)) => R = {
    case ((x1, x2)) => apply(x1, x2)
  }
  override def toString(): String = "<function2>"
}
