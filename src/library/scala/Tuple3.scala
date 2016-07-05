/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 3 elements; the canonical representation of a [[scala.Product3]].
 *
 *  @constructor  Create a new tuple with 3 elements. Note that it is more idiomatic to create a Tuple3 via `(t1, t2, t3)`
 *  @param  _1   Element 1 of this Tuple3
 *  @param  _2   Element 2 of this Tuple3
 *  @param  _3   Element 3 of this Tuple3
 */
final case class Tuple3[+T1, +T2, +T3](_1: T1, _2: T2, _3: T3)
  extends Product3[T1, T2, T3]
{
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + ")"
  
}
