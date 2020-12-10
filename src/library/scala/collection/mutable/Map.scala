/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import generic._

/** A base trait for maps that can be mutated.
 *  $mapNote
 *  $mapTags
 *  @since 1.0
 *  @author  Matthias Zenger
 */
trait Map[K, V]
  extends Iterable[(K, V)]
//     with GenMap[K, V]
     with scala.collection.Map[K, V]
     with MapLike[K, V, Map[K, V]] {

  override def empty: Map[K, V] = Map.empty

  override def seq: Map[K, V] = this

  /** The same map with a given default function.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     the function mapping keys to values, used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefault(d: K => V): mutable.Map[K, V] = new Map.WithDefault[K, V](this, d)

  /** The same map with a given default value.
   *
   *  Invoking transformer methods (e.g. `map`) will not preserve the default value.
   *
   *  @param d     default value used for non-present keys
   *  @return      a wrapper of the map with a default value
   */
  def withDefaultValue(d: V): mutable.Map[K, V] = new Map.WithDefault[K, V](this, x => d)
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `HashMap`.
 *  @define coll mutable map
 *  @define Coll `mutable.Map`
 */
object Map extends MutableMapFactory[Map] {
  /** $canBuildFromInfo */
  implicit def canBuildFrom[K, V]: CanBuildFrom[Coll, (K, V), Map[K, V]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (K, V), Map[K, V]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Nothing, Nothing]

  def empty[K, V]: Map[K, V] = new HashMap[K, V]

  @SerialVersionUID(3886083557164597477L)
  class WithDefault[K, V](underlying: Map[K, V], d: K => V) extends scala.collection.Map.WithDefault(underlying, d) with Map[K, V] {
    override def += (kv: (K, V)) = {underlying += kv; this}
    def -= (key: K) = {underlying -= key; this}
    override def empty = new WithDefault(underlying.empty, d)
    override def updated[V1 >: V](key: K, value: V1): WithDefault[K, V1] = new WithDefault[K, V1](underlying.updated[V1](key, value), d)
    override def + [V1 >: V](kv: (K, V1)): WithDefault[K, V1] = updated(kv._1, kv._2)
    override def - (key: K): WithDefault[K, V] = new WithDefault(underlying - key, d)

    /** If these methods aren't overridden to thread through the underlying map,
     *  successive calls to withDefault* have no effect.
     */
    override def withDefault(d: K => V): mutable.Map[K, V] = new WithDefault[K, V](underlying, d)
    override def withDefaultValue(d: V): mutable.Map[K, V] = new WithDefault[K, V](underlying, x => d)
  }
}

/** Explicit instantiation of the `Map` trait to reduce class file size in subclasses. */
abstract class AbstractMap[K, V] extends scala.collection.AbstractMap[K, V] with Map[K, V]
