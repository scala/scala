package scala.collection
package parallel.immutable






import scala.collection.immutable.Set
import scala.collection.generic._
import scala.collection.parallel.ParSetLike
import scala.collection.parallel.Combiner




trait ParSet[T]
extends Set[T]
   with GenericParTemplate[T, ParSet]
   with parallel.ParSet[T]
   with ParIterable[T]
   with ParSetLike[T, ParSet[T], Set[T]]
{
self =>
  override def empty: ParSet[T] = ParHashSet[T]()

  override def companion: GenericCompanion[ParSet] with GenericParCompanion[ParSet] = ParSet

  override def stringPrefix = "ParSet"

  override def toParSet[U >: T] = this.asInstanceOf[ParSet[U]]

}



object ParSet extends ParSetFactory[ParSet] {
  def newCombiner[T]: Combiner[T, ParSet[T]] = HashSetCombiner[T]

  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSet[T]] = new GenericCanCombineFrom[T]
}














