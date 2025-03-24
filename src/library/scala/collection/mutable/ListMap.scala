/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package mutable

import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.List

/** A simple mutable map backed by a list, so it preserves insertion order.
  *
  *  @tparam K    the type of the keys contained in this list map.
  *  @tparam V    the type of the values assigned to keys in this list map.
  *
  *  @define Coll `mutable.ListMap`
  *  @define coll mutable list map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  *  @define orderDependent
  *  @define orderDependentFold
  */
@deprecated("Use an immutable.ListMap assigned to a var instead of mutable.ListMap", "2.13.0")
class ListMap[K, V]
  extends AbstractMap[K, V]
    with MapOps[K, V, ListMap, ListMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, ListMap[K, V]]
    with StrictOptimizedMapOps[K, V, ListMap, ListMap[K, V]]
    with MapFactoryDefaults[K, V, ListMap, Iterable]
    with DefaultSerializable {

  override def mapFactory: MapFactory[ListMap] = ListMap

  private[this] var elems: List[(K, V)] = List()
  private[this] var siz: Int = 0

  def get(key: K): Option[V] = elems find (_._1 == key) map (_._2)
  def iterator: Iterator[(K, V)] = elems.iterator

  final override def addOne(kv: (K, V)) = {
    val (e, key0) = remove(kv._1, elems, List())
    elems = (key0, kv._2) :: e
    siz += 1; this
  }

  final override def subtractOne(key: K) = { elems = remove(key, elems, List())._1; this }

  @tailrec
  private def remove(key: K, elems: List[(K, V)], acc: List[(K, V)]): (List[(K, V)], K) = {
    if (elems.isEmpty) (acc, key)
    else if (elems.head._1 == key) { siz -= 1; (acc ::: elems.tail, elems.head._1) }
    else remove(key, elems.tail, elems.head :: acc)
  }

  final override def clear(): Unit = { elems = List(); siz = 0 }

  final override def size: Int = siz
  override def knownSize: Int = size
  override def isEmpty: Boolean = size == 0
  override protected[this] def stringPrefix = "ListMap"
}

/** $factoryInfo
  *  @define Coll `mutable.ListMap`
  *  @define coll mutable list map
  */
@SerialVersionUID(3L)
@deprecated("Use an immutable.ListMap assigned to a var instead of mutable.ListMap", "2.13.0")
object ListMap extends MapFactory[ListMap] {
  def empty[K, V]: ListMap[K, V] = new ListMap[K, V]
  def from[K, V](it: IterableOnce[(K, V)]): ListMap[K,V] = Growable.from(empty[K, V], it)
  def newBuilder[K, V]: Builder[(K, V), ListMap[K,V]] = new GrowableBuilder(empty[K, V])
}
