package scala.collection.parallel




import scala.collection.TraversableView
import scala.collection.SeqView
import scala.collection.Parallel
import scala.collection.generic.CanCombineFrom





/** A template view of a non-strict view of a parallel sequence.
 *
 *  @tparam T
 *  @tparam Coll
 *
 *  @since 2.8
 */
trait ParallelSeqView[+T, +Coll <: Parallel, +CollSeq]
extends ParallelSeqViewLike[T, Coll, CollSeq, ParallelSeqView[T, Coll, CollSeq], SeqView[T, CollSeq]]



object ParallelSeqView {
  abstract class NoCombiner[T] extends Combiner[T, Nothing] {
    self: EnvironmentPassingCombiner[T, Nothing] =>
    def +=(elem: T): this.type = this
    def iterator: Iterator[T] = Iterator.empty
    def result() = throw new UnsupportedOperationException("ParallelSeqView.Combiner.result")
    def size = throw new UnsupportedOperationException("ParallelSeqView.Combiner.size")
    def clear() {}
    def combine[N <: T, NewTo >: Nothing](other: Combiner[N, NewTo]) =
      throw new UnsupportedOperationException("ParallelSeqView.Combiner.result")
  }

  type Coll = ParallelSeqView[_, C, _] forSome { type C <: ParallelSeq[_] }

  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParallelSeqView[T, ParallelSeq[T], Seq[T]]] =
    new CanCombineFrom[Coll, T, ParallelSeqView[T, ParallelSeq[T], Seq[T]]] {
      def apply(from: Coll) = new NoCombiner[T] with EnvironmentPassingCombiner[T, Nothing]
      def apply() = new NoCombiner[T] with EnvironmentPassingCombiner[T, Nothing]
    }
}


















