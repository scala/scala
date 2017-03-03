/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 1 elements; the canonical representation of a [[scala.Product1]].
 *
 *  @constructor  Create a new tuple with 1 elements.
 *  @param  _1   Element 1 of this Tuple1
 */
final case class Tuple1[@specialized(Int, Long, Double) +T1](_1: T1)
  extends Product1[T1]
{
  override def toString() = "(" + _1 + ")"
  
}
