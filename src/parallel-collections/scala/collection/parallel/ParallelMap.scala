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

  override def empty: ParallelMap[K, V] = null // TODO

}



object ParallelMap extends ParallelMapFactory[ParallelMap] {
  def empty[A, B]: ParallelMap[A, B] = null // TODO

  implicit def canBuildFrom[A, B]: CanBuildFromParallel[Coll, (A, B), Map[A, B]] = null // TODO
}




























