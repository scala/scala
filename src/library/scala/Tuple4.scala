/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 4 elements; the canonical representation of a [[scala.Product4]].
 *
 *  @constructor  Create a new tuple with 4 elements. Note that it is more idiomatic to create a Tuple4 via `(t1, t2, t3, t4)`
 *  @param  _1   Element 1 of this Tuple4
 *  @param  _2   Element 2 of this Tuple4
 *  @param  _3   Element 3 of this Tuple4
 *  @param  _4   Element 4 of this Tuple4
 */
@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0")
case class Tuple4[+T1, +T2, +T3, +T4](_1: T1, _2: T2, _3: T3, _4: T4)
  extends Product4[T1, T2, T3, T4]
{
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + ")"
  
}
