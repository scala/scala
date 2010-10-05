package scala.collection.parallel





import scala.collection.Map
import scala.collection.mutable.Builder
import scala.collection.generic.ParMapFactory
import scala.collection.generic.GenericParMapTemplate
import scala.collection.generic.GenericParMapCompanion
import scala.collection.generic.CanCombineFrom






trait ParMap[K, +V]
extends Map[K, V]
   with GenericParMapTemplate[K, V, ParMap]
   with ParIterable[(K, V)]
   with ParMapLike[K, V, ParMap[K, V], Map[K, V]]
{
self =>

  def mapCompanion: GenericParMapCompanion[ParMap] = ParMap

  override def empty: ParMap[K, V] = new immutable.ParHashMap[K, V]

  override def stringPrefix = "ParMap"
}



object ParMap extends ParMapFactory[ParMap] {
  def empty[K, V]: ParMap[K, V] = new immutable.ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParMap[K, V]] = immutable.HashMapCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParMap[K, V]] = new CanCombineFromMap[K, V]

}




























