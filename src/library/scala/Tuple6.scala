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


/** A tuple of 6 elements; the canonical representation of a [[scala.Product6]].
 *
 *  @constructor  Create a new tuple with 6 elements. Note that it is more idiomatic to create a Tuple6 via `(t1, t2, t3, t4, t5, t6)`
 *  @tparam T1 the type of the 1st element
 *  @tparam T2 the type of the 2nd element
 *  @tparam T3 the type of the 3rd element
 *  @tparam T4 the type of the 4th element
 *  @tparam T5 the type of the 5th element
 *  @tparam T6 the type of the 6th element
 *  @param  _1   the 1st element of this Tuple6
 *  @param  _2   the 2nd element of this Tuple6
 *  @param  _3   the 3rd element of this Tuple6
 *  @param  _4   the 4th element of this Tuple6
 *  @param  _5   the 5th element of this Tuple6
 *  @param  _6   the 6th element of this Tuple6
 */
final case class Tuple6[+T1, +T2, +T3, +T4, +T5, +T6](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6)
  extends Product6[T1, T2, T3, T4, T5, T6]
{
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + ")"
  
}
