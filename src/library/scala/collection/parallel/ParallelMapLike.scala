package scala.collection.parallel




import scala.collection.MapLike
import scala.collection.Map
import scala.collection.mutable.Builder








trait ParallelMapLike[K,
		      +V,
		      +Repr <: ParallelMapLike[K, V, Repr, SequentialView] with ParallelMap[K, V],
		      +SequentialView <: Map[K, V]]
extends MapLike[K, V, Repr]
   with ParallelIterableLike[(K, V), Repr, SequentialView]
{ self =>

  protected[this] override def newBuilder: Builder[(K, V), Repr] = newCombiner

  protected[this] override def newCombiner: Combiner[(K, V), Repr] = error("Must be implemented in concrete classes.")

  override def empty: Repr

}












