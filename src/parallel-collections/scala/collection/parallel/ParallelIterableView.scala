package scala.collection.parallel




import scala.collection.Parallel
import scala.collection.TraversableViewLike
import scala.collection.IterableView




/** A template view of a non-strict view of a parallel iterable collection.
 *
 *  @tparam T         ...
 *  @tparam Coll      ...
 *
 *  @since 2.8
 */
trait ParallelIterableView[+T, +Coll <: Parallel, +CollSeq]
extends ParallelIterableViewLike[T, Coll, CollSeq, ParallelIterableView[T, Coll, CollSeq], IterableView[T, CollSeq]]












