package scala.collection.parallel.mutable



import scala.collection.generic._
import scala.collection.parallel.Combiner






/** A mutable variant of `ParSet`.
 *
 *  @define Coll mutable.ParSet
 *  @define coll mutable parallel set
 */
trait ParSet[T]
extends collection.mutable.Set[T]
   with ParIterable[T]
   with collection.parallel.ParSet[T]
   with GenericParTemplate[T, ParSet]
   with ParSetLike[T, ParSet[T], collection.mutable.Set[T]]
{
self =>
  override def companion: GenericCompanion[ParSet] with GenericParCompanion[ParSet] = ParSet;
  override def empty: ParSet[T] = ParHashSet()
}


/** $factoryInfo
 *  @define Coll mutable.ParSet
 *  @define coll mutable parallel set
 */
object ParSet extends ParSetFactory[ParSet] {
  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSet[T]] = new GenericCanCombineFrom[T]

  override def newBuilder[T]: Combiner[T, ParSet[T]] = ParHashSet.newBuilder

  override def newCombiner[T]: Combiner[T, ParSet[T]] = ParHashSet.newCombiner
}

