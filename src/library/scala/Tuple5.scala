/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 5 elements; the canonical representation of a [[scala.Product5]].
 *
 *  @constructor  Create a new tuple with 5 elements. Note that it is more idiomatic to create a Tuple5 via `(t1, t2, t3, t4, t5)`
 *  @param  _1   Element 1 of this Tuple5
 *  @param  _2   Element 2 of this Tuple5
 *  @param  _3   Element 3 of this Tuple5
 *  @param  _4   Element 4 of this Tuple5
 *  @param  _5   Element 5 of this Tuple5
 */
final case class Tuple5[+T1, +T2, +T3, +T4, +T5](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5)
  extends Product5[T1, T2, T3, T4, T5]
{
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + ")"
  
}
