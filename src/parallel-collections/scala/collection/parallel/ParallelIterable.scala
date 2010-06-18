package scala.collection.parallel


import scala.collection.generic._
import scala.collection.parallel.mutable.ParallelArrayCombiner
import scala.collection.parallel.mutable.ParallelArray


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
trait ParallelIterable[+T] extends Iterable[T]
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














