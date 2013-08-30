/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package parallel.mutable

import scala.collection.generic._
import scala.collection.parallel.Combiner

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
extends GenMap[K, V]
   with parallel.ParMap[K, V]
   with ParIterable[(K, V)]
   with GenericParMapTemplate[K, V, ParMap]
   with ParMapLike[K, V, ParMap[K, V], mutable.Map[K, V]]
{

  protected[this] override def newCombiner: Combiner[(K, V), ParMap[K, V]] = ParMap.newCombiner[K, V]

  override def mapCompanion: GenericParMapCompanion[ParMap] = ParMap

  override def empty: ParMap[K, V] = new ParHashMap[K, V]

  def seq: scala.collection.mutable.Map[K, V]

  override def updated [U >: V](key: K, value: U): ParMap[K, U] = this + ((key, value))

  /** The same map with a given default function.
   *  Note: `get`, `contains`, `iterator`, `keys`, etc are not affected by `withDefault`.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefault(d: K => V): scala.collection.parallel.mutable.ParMap[K, V] = new ParMap.WithDefault[K, V](this, d)

  /** The same map with a given default value.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefaultValue(d: V): scala.collection.parallel.mutable.ParMap[K, V] = new ParMap.WithDefault[K, V](this, x => d)
}

object ParMap extends ParMapFactory[ParMap] {
  def empty[K, V]: ParMap[K, V] = new ParHashMap[K, V]

  def newCombiner[K, V]: Combiner[(K, V), ParMap[K, V]] = ParHashMapCombiner.apply[K, V]

  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParMap[K, V]] = new CanCombineFromMap[K, V]

  class WithDefault[K, V](underlying: ParMap[K, V], d: K => V)
  extends scala.collection.parallel.ParMap.WithDefault(underlying, d) with ParMap[K, V] {
    override def += (kv: (K, V)) = {underlying += kv; this}
    def -= (key: K) = {underlying -= key; this}
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[U >: V](key: K, value: U): WithDefault[K, U] = new WithDefault[K, U](underlying.updated[U](key, value), d)
    override def + [U >: V](kv: (K, U)): WithDefault[K, U] = updated(kv._1, kv._2)
    override def - (key: K): WithDefault[K, V] = new WithDefault(underlying - key, d)
    override def seq = underlying.seq.withDefault(d)
    def clear() = underlying.clear()
    def put(key: K, value: V): Option[V] = underlying.put(key, value)

    /** If these methods aren't overridden to thread through the underlying map,
     *  successive calls to withDefault* have no effect.
     */
    override def withDefault(d: K => V): ParMap[K, V] = new WithDefault[K, V](underlying, d)
    override def withDefaultValue(d: V): ParMap[K, V] = new WithDefault[K, V](underlying, x => d)
  }
}
