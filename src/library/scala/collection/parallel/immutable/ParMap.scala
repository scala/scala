/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package parallel.immutable





import scala.collection.immutable.Map
import scala.collection.generic.ParMapFactory
import scala.collection.generic.GenericParMapTemplate
import scala.collection.generic.GenericParMapCompanion
import scala.collection.generic.CanCombineFrom
import scala.collection.parallel.ParMapLike
import scala.collection.parallel.Combiner
import scala.collection.GenMapLike





/** A template trait for immutable parallel maps.
 *
 *  $sideeffects
 *
 *  @tparam K    the key type of the map
 *  @tparam V    the value type of the map
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParMap[K, +V]
extends collection/*.immutable*/.GenMap[K, V]
   with GenericParMapTemplate[K, V, ParMap]
   with parallel.ParMap[K, V]
   with ParIterable[(K, V)]
   with ParMapLike[K, V, ParMap[K, V], Map[K, V]]
{
self =>

  override def mapCompanion: GenericParMapCompanion[ParMap] = ParMap

  override def empty: ParMap[K, V] = new ParHashMap[K, V]

  override def stringPrefix = "ParMap"

  override def toMap[P, Q](implicit ev: (K, V) <:< (P, Q)): ParMap[P, Q] = this.asInstanceOf[ParMap[P, Q]]
}



object ParMap extends ParMapFactory[ParMap] {
  def empty[K, V]: ParMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParMap[K, V]] = HashMapCombiner[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParMap[K, V]] = new CanCombineFromMap[K, V]

}



















