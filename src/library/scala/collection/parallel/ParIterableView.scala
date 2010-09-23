package scala.collection.parallel




import scala.collection.Parallel
import scala.collection.TraversableViewLike
import scala.collection.IterableView
import scala.collection.generic.CanCombineFrom




/** A template view of a non-strict view of a parallel iterable collection.
 *
 *  @tparam T         ...
 *  @tparam Coll      ...
 *
 *  @since 2.8
 */
trait ParIterableView[+T, +Coll <: Parallel, +CollSeq]
extends ParIterableViewLike[T, Coll, CollSeq, ParIterableView[T, Coll, CollSeq], IterableView[T, CollSeq]]




object ParIterableView {
  abstract class NoCombiner[T] extends Combiner[T, Nothing] {
    self: EnvironmentPassingCombiner[T, Nothing] =>
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
      def apply(from: Coll) = new NoCombiner[T] with EnvironmentPassingCombiner[T, Nothing]
      def apply() = new NoCombiner[T] with EnvironmentPassingCombiner[T, Nothing]
    }
}










