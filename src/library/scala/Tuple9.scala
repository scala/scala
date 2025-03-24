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


/** A tuple of 9 elements; the canonical representation of a [[scala.Product9]].
 *
 *  @constructor  Create a new tuple with 9 elements. Note that it is more idiomatic to create a Tuple9 via `(t1, t2, t3, t4, t5, t6, t7, t8, t9)`
 *  @param  _1   Element 1 of this Tuple9
 *  @param  _2   Element 2 of this Tuple9
 *  @param  _3   Element 3 of this Tuple9
 *  @param  _4   Element 4 of this Tuple9
 *  @param  _5   Element 5 of this Tuple9
 *  @param  _6   Element 6 of this Tuple9
 *  @param  _7   Element 7 of this Tuple9
 *  @param  _8   Element 8 of this Tuple9
 *  @param  _9   Element 9 of this Tuple9
 */
final case class Tuple9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9)
  extends Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9]
{
  override def toString(): String = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + "," + _7 + "," + _8 + "," + _9 + ")"
  
}
