/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel.mutable




import collection.generic._
import collection.parallel.Combiner



/** A template trait for mutable parallel maps.
 *
 *  $sideeffects
 *
 *  @tparam K    the key type of the map
 *  @tparam V    the value type of the map
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParMap[K, V]
extends collection.mutable.GenMap[K, V]
   with collection.parallel.ParMap[K, V]
   with /* mutable */ ParIterable[(K, V)]
   with GenericParMapTemplate[K, V, ParMap]
   with /* mutable */ ParMapLike[K, V, ParMap[K, V], collection.mutable.Map[K, V]]
{

  protected[this] override def newCombiner: Combiner[(K, V), ParMap[K, V]] = ParMap.newCombiner[K, V]

  override def mapCompanion: GenericParMapCompanion[ParMap] = ParMap

  override def empty: ParMap[K, V] = new ParHashMap[K, V]

  def seq: collection.mutable.Map[K, V]

}



object ParMap extends ParMapFactory[ParMap] {
  def empty[K, V]: ParMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParMap[K, V]] = ParHashMapCombiner.apply[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParMap[K, V]] = new CanCombineFromMap[K, V]

}



















