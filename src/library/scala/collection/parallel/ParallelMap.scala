package scala.collection.parallel





import scala.collection.Map
import scala.collection.mutable.Builder
import scala.collection.generic.ParallelMapFactory
import scala.collection.generic.GenericParallelMapTemplate
import scala.collection.generic.GenericParallelMapCompanion
import scala.collection.generic.CanCombineFrom






trait ParallelMap[K, +V]
extends Map[K, V]
   with GenericParallelMapTemplate[K, V, ParallelMap]
   with ParallelIterable[(K, V)]
   with ParallelMapLike[K, V, ParallelMap[K, V], Map[K, V]]
{
self =>

  def mapCompanion: GenericParallelMapCompanion[ParallelMap] = ParallelMap

  override def empty: ParallelMap[K, V] = new immutable.ParallelHashTrie[K, V]

  override def stringPrefix = "ParallelMap"
}



object ParallelMap extends ParallelMapFactory[ParallelMap] {
  def empty[K, V]: ParallelMap[K, V] = new immutable.ParallelHashTrie[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParallelMap[K, V]] = immutable.HashTrieCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParallelMap[K, V]] = new CanCombineFromMap[K, V]

}




























