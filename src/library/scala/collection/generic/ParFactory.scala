/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import scala.collection.parallel.ParIterable
import scala.language.higherKinds

/** A template class for companion objects of `ParIterable` and subclasses
 *  thereof. This class extends `TraversableFactory` and provides a set of
 *  operations to create `$Coll` objects.
 *
 *  @define coll parallel collection
 *  @define Coll `ParIterable`
 *  @since 2.8
 */
abstract class ParFactory[CC[X] <: ParIterable[X] with GenericParTemplate[X, CC]]
extends GenTraversableFactory[CC]
   with GenericParCompanion[CC] {

  //type EPC[T, C] = scala.collection.parallel.EnvironmentPassingCombiner[T, C]

  /** A generic implementation of the `CanCombineFrom` trait, which forwards
   *  all calls to `apply(from)` to the `genericParBuilder` method of the $coll
   * `from`, and calls to `apply()` to this factory.
   */
  class GenericCanCombineFrom[A] extends GenericCanBuildFrom[A] with CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner
    override def apply() = newBuilder[A]
  }
}
