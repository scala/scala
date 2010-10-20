package scala.collection.parallel.mutable




import collection.generic._
import collection.parallel.Combiner



trait ParMap[K, V]
extends collection.mutable.Map[K, V]
   with collection.parallel.ParMap[K, V]
   with /* mutable */ ParIterable[(K, V)]
   with GenericParMapTemplate[K, V, ParMap]
   with /* mutable */ ParMapLike[K, V, ParMap[K, V], collection.mutable.Map[K, V]]
{

  override def mapCompanion: GenericParMapCompanion[ParMap] = ParMap

  override def empty: ParMap[K, V] = new ParHashMap[K, V]

}



object ParMap extends ParMapFactory[ParMap] {
  def empty[K, V]: ParMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParMap[K, V]] = ParHashMapCombiner.apply[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParMap[K, V]] = new CanCombineFromMap[K, V]

}



















