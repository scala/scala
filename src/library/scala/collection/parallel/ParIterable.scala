package scala.collection.parallel


import scala.collection.generic._
import scala.collection.parallel.mutable.ParArrayCombiner
import scala.collection.parallel.mutable.ParArray


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
trait ParIterable[+T] extends Iterable[T]
                         with GenericParTemplate[T, ParIterable]
                         with ParIterableLike[T, ParIterable[T], Iterable[T]] {
  override def companion: GenericCompanion[ParIterable] with GenericParCompanion[ParIterable] = ParIterable
}

/** $factoryinfo
 */
object ParIterable extends ParFactory[ParIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterable[T]] = new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]

  def newCombiner[T]: Combiner[T, ParIterable[T]] = ParArrayCombiner[T]
}














