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


/** A function of 6 parameters.
 *
 */
trait Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6) == apply(x1, x2, x3, x4, x5, x6)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6) => self.apply(x1, x2, x3, x4, x5, x6)).curried
  }
  /** Creates a tupled version of this function: instead of 6 arguments,
   *  it accepts a single [[scala.Tuple6]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6)) == f(Tuple6(x1, x2, x3, x4, x5, x6)) == apply(x1, x2, x3, x4, x5, x6)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6)) => R = {
    case ((x1, x2, x3, x4, x5, x6)) => apply(x1, x2, x3, x4, x5, x6)
  }
  override def toString(): String = "<function6>"
}
