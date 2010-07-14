package scala.collection.generic



import scala.collection.parallel.ParallelMap
import scala.collection.parallel.ParallelMapLike
import scala.collection.parallel.Combiner
import scala.collection.mutable.Builder




/** A template class for companion objects of `ParallelMap` and subclasses thereof.
 *  This class extends `TraversableFactory` and provides a set of operations to create `$Coll` objects.
 *
 *  @define $coll parallel map
 *  @define $Coll ParallelMap
 */
abstract class ParallelMapFactory[CC[X, Y] <: ParallelMap[X, Y] with ParallelMapLike[X, Y, CC[X, Y], _]]
extends MapFactory[CC] {

  /** The default builder for $Coll objects.
   *  @tparam K      the type of the keys
   *  @tparam V      the type of the associated values
   */
  override def newBuilder[K, V]: Builder[(K, V), CC[K, V]] = newCombiner[K, V]

  /** The default combiner for $Coll objects.
   *  @tparam K     the type of the keys
   *  @tparam V     the type of the associated values
   */
  def newCombiner[K, V]: Combiner[(K, V), CC[K, V]] = null // TODO

  class ParallelMapCanBuildFrom[K, V] extends CanBuildFromParallel[CC[_, _], (K, V), CC[K, V]] {
    def apply(from: CC[_, _]) = newCombiner[K, V]
    def apply() = newCombiner[K, V]
  }

}
