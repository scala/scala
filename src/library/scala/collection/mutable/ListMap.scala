/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import scala.annotation.tailrec
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
class ListMap[K, V]
  extends AbstractMap[K, V]
    with MapOps[K, V, ListMap, ListMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, ListMap[K, V]]
    with Serializable {

  override def mapFactory: MapFactory[ListMap] = ListMap

  private var elems: List[(K, V)] = List()
  private var siz: Int = 0

  def get(key: K): Option[V] = elems find (_._1 == key) map (_._2)
  def iterator(): Iterator[(K, V)] = elems.iterator()

  final override def addOne(kv: (K, V)) = { elems = remove(kv._1, elems, List()); elems = kv :: elems; siz += 1; this }

  final override def subtractOne(key: K) = { elems = remove(key, elems, List()); this }

  @tailrec
  private def remove(key: K, elems: List[(K, V)], acc: List[(K, V)]): List[(K, V)] = {
    if (elems.isEmpty) acc
    else if (elems.head._1 == key) { siz -= 1; acc ::: elems.tail }
    else remove(key, elems.tail, elems.head :: acc)
  }

  final override def clear(): Unit = { elems = List(); siz = 0 }

  final override def size: Int = siz
}

/** $factoryInfo
  *  @define Coll `mutable.ListMap`
  *  @define coll mutable list map
  */
object ListMap extends MapFactory[ListMap] {
  def empty[K, V]: ListMap[K, V] = new ListMap[K, V]
  def from[K, V](it: IterableOnce[(K, V)]): ListMap[K,V] = Growable.from(empty[K, V], it)
  def newBuilder[K, V](): Builder[(K, V), ListMap[K,V]] = new GrowableBuilder(empty[K, V])
}
