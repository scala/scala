/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.{ Parallel, IterableView, GenIterableView, Iterator }
import scala.collection.generic.CanCombineFrom

/** A template view of a non-strict view of a parallel iterable collection.
 *
 *  @tparam T         the type of elements
 *  @tparam Coll      the type of the parallel collection this view was created from
 *  @tparam CollSeq   the type of the sequential collection corresponding to the underlying parallel collection
 *
 *  @since 2.9
 */
trait ParIterableView[+T, +Coll <: Parallel, +CollSeq]
extends ParIterableViewLike[T, Coll, CollSeq, ParIterableView[T, Coll, CollSeq], IterableView[T, CollSeq]]
   with GenIterableView[T, Coll]


object ParIterableView {
  abstract class NoCombiner[T] extends Combiner[T, Nothing] {
//    self: EnvironmentPassingCombiner[T, Nothing] =>
    def +=(elem: T): this.type = this
    def iterator: Iterator[T] = Iterator.empty
    def result() = throw new UnsupportedOperationException("ParIterableView.Combiner.result")
    def size = throw new UnsupportedOperationException("ParIterableView.Combiner.size")
    def clear() {}
    def combine[N <: T, NewTo >: Nothing](other: Combiner[N, NewTo]) =
      throw new UnsupportedOperationException("ParIterableView.Combiner.result")
  }

  type Coll = ParIterableView[_, C, _] forSome { type C <: ParIterable[_] }

  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterableView[T, ParIterable[T], Iterable[T]]] =
    new CanCombineFrom[Coll, T, ParIterableView[T, ParIterable[T], Iterable[T]]] {
      def apply(from: Coll) = new NoCombiner[T] {} // was: with EnvironmentPassingCombiner[T, Nothing]
      def apply() = new NoCombiner[T] {} // was: with EnvironmentPassingCombiner[T, Nothing]
    }
}
