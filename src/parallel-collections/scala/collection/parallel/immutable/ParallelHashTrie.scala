package scala.collection.parallel.immutable







import scala.collection.parallel.ParallelMap
import scala.collection.parallel.ParallelMapLike
import scala.collection.immutable.HashMap








class ParallelHashTrie[K, +V]
extends ParallelMap[K, V]
   with ParallelMapLike[K, V, ParallelHashTrie[K, V], HashMap[K, V]]
{ self =>

  override def empty: ParallelHashTrie[K, V] = null // TODO

  def parallelIterator = null // TODO

  def seq = null // TODO

  def -(k: K) = null // TODO

  def +[U >: V](kv: (K, U)) = null // TODO

  def get(k: K) = None // TODO

}





object ParallelHashTrie {

}















