package scala.collection.parallel







import scala.collection.Set
import scala.collection.mutable.Builder
import scala.collection.generic._






trait ParSet[T]
extends Set[T]
   with GenericParTemplate[T, ParSet]
   with ParIterable[T]
   with ParSetLike[T, ParSet[T], Set[T]]
{
self =>
  override def empty: ParSet[T] = immutable.ParHashSet[T]()

  override def companion: GenericCompanion[ParSet] with GenericParCompanion[ParSet] = ParSet

  override def stringPrefix = "ParSet"
}



object ParSet extends ParSetFactory[ParSet] {
  def newCombiner[T]: Combiner[T, ParSet[T]] = immutable.HashSetCombiner[T]

  implicit def canBuildFrom[T]: CanCombineFrom[Coll, T, ParSet[T]] = new GenericCanCombineFrom[T]
}













































