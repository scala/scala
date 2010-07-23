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
trait ParSeqView[+T, +Coll <: Parallel, +CollSeq]
extends ParSeqViewLike[T, Coll, CollSeq, ParSeqView[T, Coll, CollSeq], SeqView[T, CollSeq]]



object ParSeqView {
  abstract class NoCombiner[T] extends Combiner[T, Nothing] {
    self: EnvironmentPassingCombiner[T, Nothing] =>
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
      def apply(from: Coll) = new NoCombiner[T] with EnvironmentPassingCombiner[T, Nothing]
      def apply() = new NoCombiner[T] with EnvironmentPassingCombiner[T, Nothing]
    }
}


















