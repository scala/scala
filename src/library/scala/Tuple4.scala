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


/** A tuple of 4 elements; the canonical representation of a [[scala.Product4]].
 *
 *  @constructor  Create a new tuple with 4 elements. Note that it is more idiomatic to create a Tuple4 via `(t1, t2, t3, t4)`
 *  @tparam T1 the type of the 1st element
 *  @tparam T2 the type of the 2nd element
 *  @tparam T3 the type of the 3rd element
 *  @tparam T4 the type of the 4th element
 *  @param  _1   element 1 of this Tuple4
 *  @param  _2   element 2 of this Tuple4
 *  @param  _3   element 3 of this Tuple4
 *  @param  _4   element 4 of this Tuple4
 */
final case class Tuple4[+T1, +T2, +T3, +T4](_1: T1, _2: T2, _3: T3, _4: T4)
  extends Product4[T1, T2, T3, T4]
{
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + ")"
  
}
