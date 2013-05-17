/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.{ SeqView, Parallel, Iterator }
import scala.collection.generic.CanCombineFrom

/** A template view of a non-strict view of a parallel sequence.
 *
 *  @tparam T         the type of elements in this parallel sequence
 *  @tparam Coll      the type of the underlying parallel collection
 *  @tparam CollSeq   the type of the sequential collection corresponding to the underlying parallel collection
 *
 *  @since 2.9
 */
trait ParSeqView[+T, +Coll <: Parallel, +CollSeq]
extends ParSeqViewLike[T, Coll, CollSeq, ParSeqView[T, Coll, CollSeq], SeqView[T, CollSeq]]


object ParSeqView {
  abstract class NoCombiner[T] extends Combiner[T, Nothing] {
//    self: EnvironmentPassingCombiner[T, Nothing] =>
    def +=(elem: T): this.type = this
    def iterator: Iterator[T] = Iterator.empty
    def result() = throw new UnsupportedOperationException("ParSeqView.Combiner.result")
    def size = throw new UnsupportedOperationException("ParSeqView.Combiner.size")
    def clear() {}
    def combine[N <: T, NewTo >: Nothing](other: Combiner[N, NewTo]) =
      throw new UnsupportedOperationException("ParSeqView.Combiner.result")
  }

  type Coll = ParSeqView[_, C, _] forSome { type C <: ParSeq[_] }

  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSeqView[T, ParSeq[T], Seq[T]]] =
    new CanCombineFrom[Coll, T, ParSeqView[T, ParSeq[T], Seq[T]]] {
      def apply(from: Coll) = new NoCombiner[T] {} // was: with EnvironmentPassingCombiner[T, Nothing]
      def apply() = new NoCombiner[T] {} // was: with EnvironmentPassingCombiner[T, Nothing]
    }
}
