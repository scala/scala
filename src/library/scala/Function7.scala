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

// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A function of 7 parameters.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam T2 the type of the 2nd argument
 *  @tparam T3 the type of the 3rd argument
 *  @tparam T4 the type of the 4th argument
 *  @tparam T5 the type of the 5th argument
 *  @tparam T6 the type of the 6th argument
 *  @tparam T7 the type of the 7th argument
 *  @tparam R the return type of this function
 */
trait Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends AnyRef { self =>
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the value of the 1st argument
   *  @param v2 the value of the 2nd argument
   *  @param v3 the value of the 3rd argument
   *  @param v4 the value of the 4th argument
   *  @param v5 the value of the 5th argument
   *  @param v6 the value of the 6th argument
   *  @param v7 the value of the 7th argument
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7): R
  /** Apply the body of this function to the arguments which are taken from the implicit context.
   *  @return   the result of function application.
   */
  @annotation.unspecialized def applyToContext(implicit v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7): R = apply(v1, v2, v3, v4, v5, v6, v7)
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7) == apply(x1, x2, x3, x4, x5, x6, x7)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7) => self.apply(x1, x2, x3, x4, x5, x6, x7)).curried
  }
  /** Creates a tupled version of this function: instead of 7 arguments,
   *  it accepts a single [[scala.Tuple7]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7)) == f(Tuple7(x1, x2, x3, x4, x5, x6, x7)) == apply(x1, x2, x3, x4, x5, x6, x7)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7)) => R = {
    ({ case ((x1, x2, x3, x4, x5, x6, x7)) => apply(x1, x2, x3, x4, x5, x6, x7) }: ((T1, T2, T3, T4, T5, T6, T7)) => R)
  }
  override def toString(): String = "<function7>"
}
