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


/** A function of 14 parameters.
 *
 *  @tparam T1 the type of the 1st argument
 *  @tparam T2 the type of the 2nd argument
 *  @tparam T3 the type of the 3rd argument
 *  @tparam T4 the type of the 4th argument
 *  @tparam T5 the type of the 5th argument
 *  @tparam T6 the type of the 6th argument
 *  @tparam T7 the type of the 7th argument
 *  @tparam T8 the type of the 8th argument
 *  @tparam T9 the type of the 9th argument
 *  @tparam T10 the type of the 10th argument
 *  @tparam T11 the type of the 11th argument
 *  @tparam T12 the type of the 12th argument
 *  @tparam T13 the type of the 13th argument
 *  @tparam T14 the type of the 14th argument
 *  @tparam R the return type of this function
 */
trait Function14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends AnyRef { self =>
  /** Applies the body of this function to the arguments.
   *
   *  @param v1 the value of the 1st argument
   *  @param v2 the value of the 2nd argument
   *  @param v3 the value of the 3rd argument
   *  @param v4 the value of the 4th argument
   *  @param v5 the value of the 5th argument
   *  @param v6 the value of the 6th argument
   *  @param v7 the value of the 7th argument
   *  @param v8 the value of the 8th argument
   *  @param v9 the value of the 9th argument
   *  @param v10 the value of the 10th argument
   *  @param v11 the value of the 11th argument
   *  @param v12 the value of the 12th argument
   *  @param v13 the value of the 13th argument
   *  @param v14 the value of the 14th argument
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14): R
  /** Apply the body of this function to the arguments which are taken from the implicit context.
   *  @return   the result of function application.
   */
  @annotation.unspecialized def applyToContext(implicit v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14): R = apply(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(x14) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => T8 => T9 => T10 => T11 => T12 => T13 => T14 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14) => self.apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)).curried
  }
  /** Creates a tupled version of this function: instead of 14 arguments,
   *  it accepts a single [[scala.Tuple14]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)) == f(Tuple14(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)) == apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)`
   */

  @annotation.unspecialized def tupled: ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) => R = {
    ({ case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)) => apply(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) }: ((T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) => R)
  }
  override def toString(): String = "<function14>"
}
