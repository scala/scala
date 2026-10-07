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


/** A tuple of 10 elements; the canonical representation of a [[scala.Product10]].
 *
 *  @constructor  Create a new tuple with 10 elements. Note that it is more idiomatic to create a Tuple10 via `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)`
 *  @tparam T1 the type of the 1st element
 *  @tparam T2 the type of the 2nd element
 *  @tparam T3 the type of the 3rd element
 *  @tparam T4 the type of the 4th element
 *  @tparam T5 the type of the 5th element
 *  @tparam T6 the type of the 6th element
 *  @tparam T7 the type of the 7th element
 *  @tparam T8 the type of the 8th element
 *  @tparam T9 the type of the 9th element
 *  @tparam T10 the type of the 10th element
 *  @param  _1   element 1 of this Tuple10
 *  @param  _2   element 2 of this Tuple10
 *  @param  _3   element 3 of this Tuple10
 *  @param  _4   element 4 of this Tuple10
 *  @param  _5   element 5 of this Tuple10
 *  @param  _6   element 6 of this Tuple10
 *  @param  _7   element 7 of this Tuple10
 *  @param  _8   element 8 of this Tuple10
 *  @param  _9   element 9 of this Tuple10
 *  @param  _10   element 10 of this Tuple10
 */
final case class Tuple10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10)
  extends Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
{
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + "," + _7 + "," + _8 + "," + _9 + "," + _10 + ")"
  
}
