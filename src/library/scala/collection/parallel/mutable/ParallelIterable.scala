package scala.collection.parallel.mutable


import scala.collection.generic._

import scala.collection.parallel.ParallelIterableLike
import scala.collection.parallel.Combiner


/** A template trait for parallel iterable collections.
 *
 *  $paralleliterableinfo
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the collection
 *
 *  @author prokopec
 *  @since 2.8
 */
trait ParallelIterable[T] extends collection.mutable.Iterable[T]
                              with collection.parallel.ParallelIterable[T]
                              with GenericParallelTemplate[T, ParallelIterable]
                              with ParallelIterableLike[T, ParallelIterable[T], Iterable[T]] {
  override def companion: GenericCompanion[ParallelIterable] with GenericParallelCompanion[ParallelIterable] = ParallelIterable
}

/** $factoryinfo
 */
object ParallelIterable extends ParallelFactory[ParallelIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParallelIterable[T]] =
    new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParallelIterable[T]] = ParallelArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParallelIterable[T]] = ParallelArrayCombiner[T]
}














