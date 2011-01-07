package scala.collection
package parallel.immutable


import scala.collection.generic._

import scala.collection.parallel.ParIterableLike
import scala.collection.parallel.Combiner


/** A template trait for immutable parallel iterable collections.
 *
 *  $paralleliterableinfo
 *
 *  $sideeffects
 *
 *  @tparam A    the element type of the collection
 *
 *  @author prokopec
 *  @since 2.8
 */
trait ParIterable[+T]
extends collection.immutable.Iterable[T]
   with collection.parallel.ParIterable[T]
   with GenericParTemplate[T, ParIterable]
   with ParIterableLike[T, ParIterable[T], Iterable[T]]
{
  override def companion: GenericCompanion[ParIterable] with GenericParCompanion[ParIterable] = ParIterable

  override def toParIterable: ParIterable[T] = this

  // override def toParSeq: ParSeq TODO vector

  override def toParSet[U >: T]: ParSet[U] = toParCollection[U, ParHashSet[U]](() => HashSetCombiner[U])

  override def toParMap[K, V](implicit ev: T <:< (K, V)): ParMap[K, V] = toParMap(() => HashMapCombiner[K, V])

}

/** $factoryinfo
 */
object ParIterable extends ParFactory[ParIterable] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParIterable[T]] =
    new GenericCanCombineFrom[T]

  def newBuilder[T]: Combiner[T, ParIterable[T]] = ParVector.newBuilder[T]

  def newCombiner[T]: Combiner[T, ParIterable[T]] = ParVector.newCombiner[T]

}














