package scala.collection.parallel





import scala.collection.Map
import scala.collection.mutable.Builder
import scala.collection.generic.ParallelMapFactory
import scala.collection.generic.CanBuildFromParallel






trait ParallelMap[K, +V]
extends Map[K, V]
   with ParallelIterable[(K, V)]
   with ParallelMapLike[K, V, ParallelMap[K, V], Map[K, V]]
{ self =>

  override def empty: ParallelMap[K, V] = new immutable.ParallelHashTrie[K, V]

  override def stringPrefix = "ParallelMap"

}



object ParallelMap extends ParallelMapFactory[ParallelMap] {
  def empty[K, V]: ParallelMap[K, V] = new immutable.ParallelHashTrie[K, V]

  implicit def canBuildFrom[K, V]: CanBuildFromParallel[Coll, (K, V), ParallelMap[K, V]] = new ParallelMapCanBuildFrom[K, V]

}




























