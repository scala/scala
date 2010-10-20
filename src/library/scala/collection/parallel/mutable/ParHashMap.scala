package scala.collection.parallel
package mutable




import collection.generic._





class ParHashMap[K, V]
extends ParMap[K, V]
   with GenericParMapTemplate[K, V, ParHashMap]
   with ParMapLike[K, V, ParHashMap[K, V], collection.mutable.HashMap[K, V]]
{
self =>

  override def mapCompanion: GenericParMapCompanion[ParHashMap] = ParHashMap

  override def empty: ParHashMap[K, V] = new ParHashMap[K, V]

  def parallelIterator = null // TODO

  def seq = null // TODO

  def get(k: K): Option[V] = null // TODO

  def +=(kv: (K, V)) = null // TODO

  def -=(k: K) = null // TODO

  override def size = 0 // TODO

}




object ParHashMap extends ParMapFactory[ParHashMap] {

  def empty[K, V]: ParHashMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParHashMap[K, V]] = null // TODO

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParHashMap[K, V]] = null // TODO

}


















