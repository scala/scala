package scala.collection.parallel




import scala.collection.MapLike
import scala.collection.Map
import scala.collection.mutable.Builder








trait ParMapLike[K,
                 +V,
                 +Repr <: ParMapLike[K, V, Repr, Sequential] with ParMap[K, V],
                 +Sequential <: Map[K, V] with MapLike[K, V, Sequential]]
extends MapLike[K, V, Repr]
   with ParIterableLike[(K, V), Repr, Sequential]
{ self =>

  protected[this] override def newBuilder: Builder[(K, V), Repr] = newCombiner

  protected[this] override def newCombiner: Combiner[(K, V), Repr] = unsupportedop("Must implement `newCombiner` in concrete collections.")

  override def empty: Repr

}












