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
package immutable

import scala.annotation.tailrec

import scala.collection.mutable.Builder
import scala.runtime.Statics.releaseFence

/**
  * This class implements immutable maps using a list-based data structure. List map iterators and
  * traversal methods visit key-value pairs in the order they were first inserted.
  *
  * Entries are stored internally in reversed insertion order, which means the newest key is at the
  * head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and `init`
  * are O(1). Other operations, such as inserting or removing entries, are also O(n), which makes
  * this collection suitable only for a small number of elements.
  *
  * Instances of `ListMap` represent empty maps; they can be either created by calling the
  * constructor directly, or by applying the function `ListMap.empty`.
  *
  * @tparam K the type of the keys contained in this list map
  * @tparam V the type of the values associated with the keys
  *
  * @author Matthias Zenger
  * @author Martin Odersky
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
sealed class ListMap[K, +V]
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, ListMap, ListMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, ListMap[K, V]]
    with StrictOptimizedMapOps[K, V, ListMap, ListMap[K, V]] {

  override def mapFactory: MapFactory[ListMap] = ListMap

  override def size: Int = 0

  override def isEmpty: Boolean = true

  override def knownSize: Int = 0
  def get(key: K): Option[V] = None

  def updated[V1 >: V](key: K, value: V1): ListMap[K, V1] = new ListMap.Node[K, V1](key, value, this)

  def removed(key: K): ListMap[K, V] = this

  def iterator: Iterator[(K, V)] = {
    var curr: ListMap[K, V] = this
    var res: List[(K, V)] = Nil
    while (curr.nonEmpty) {
      res = (curr.key, curr.value) :: res
      curr = curr.next
    }
    res.iterator
  }

  override def keys: Iterable[K] = {
    var curr: ListMap[K, V] = this
    var res: List[K] = Nil
    while (curr.nonEmpty) {
      res = curr.key :: res
      curr = curr.next
    }
    res
  }

  private[immutable] def key: K = throw new NoSuchElementException("key of empty map")
  private[immutable] def value: V = throw new NoSuchElementException("value of empty map")
  private[immutable] def next: ListMap[K, V] = throw new NoSuchElementException("next of empty map")

  override protected[this] def className = "ListMap"

}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list map with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#list-maps "Scala's Collection Library overview"]]
  * section on `List Maps` for more information.
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  */
@SerialVersionUID(3L)
object ListMap extends MapFactory[ListMap] {
  /**
    * Represents an entry in the `ListMap`.
    */
  private[immutable] class Node[K, V](
    override private[immutable] val key: K,
    override private[immutable] val value: V,
    private[immutable] var _init: ListMap[K, V]
  ) extends ListMap[K, V] {

    releaseFence()

    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[K, V], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def knownSize: Int = -1

    @throws[NoSuchElementException]
    override def apply(k: K): V = applyInternal(this, k)

    @tailrec private[this] def applyInternal(cur: ListMap[K, V], k: K): V =
      if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else if (k == cur.key) cur.value
      else applyInternal(cur.next, k)

    override def get(k: K): Option[V] = getInternal(this, k)

    @tailrec private[this] def getInternal(cur: ListMap[K, V], k: K): Option[V] =
      if (cur.isEmpty) None
      else if (k == cur.key) Some(cur.value)
      else getInternal(cur.next, k)

    override def contains(k: K): Boolean = containsInternal(this, k)

    @tailrec private[this] def containsInternal(cur: ListMap[K, V], k: K): Boolean =
      if (cur.isEmpty) false
      else if (k == cur.key) true
      else containsInternal(cur.next, k)

    override def updated[V1 >: V](k: K, v: V1): ListMap[K, V1] = {
      val (m, k0) = removeInternal(k, this, Nil)
      new Node(k0, v, m)
    }

    @tailrec private[this] def removeInternal(k: K, cur: ListMap[K, V], acc: List[ListMap[K, V]]): (ListMap[K, V], K) =
      if (cur.isEmpty) (acc.last, k)
      else if (k == cur.key) (acc.foldLeft(cur.next) { (t, h) => new Node(h.key, h.value, t) }, cur.key)
      else removeInternal(k, cur.next, cur :: acc)

    override def removed(k: K): ListMap[K, V] = removeInternal(k, this, Nil)._1

    override private[immutable] def next: ListMap[K, V] = _init

    override def last: (K, V) = (key, value)
    override def init: ListMap[K, V] = next

  }

  def empty[K, V]: ListMap[K, V] = EmptyListMap.asInstanceOf[ListMap[K, V]]

  private object EmptyListMap extends ListMap[Any, Nothing]

  def from[K, V](it: collection.IterableOnce[(K, V)]): ListMap[K, V] =
    it match {
      case lm: ListMap[K, V] => lm
      case _ => (newBuilder[K, V] ++= it).result()
    }

  /** Returns a new ListMap builder
    *
    * The implementation safely handles additions after `result()` without calling `clear()`
    *
    * @tparam K the map key type
    * @tparam V the map value type
    */
  def newBuilder[K, V]: Builder[(K, V), ListMap[K, V]] = new ListMapBuilder[K, V]
}

private[immutable] final class ListMapBuilder[K, V] extends mutable.Builder[(K, V), ListMap[K, V]] {
  private[this] var isAliased: Boolean = false
  private[this] var underlying: ListMap[K, V] = ListMap.empty

  override def clear(): Unit = {
    underlying = ListMap.empty
    isAliased = false
  }

  override def result(): ListMap[K, V] = {
    isAliased = true
    releaseFence()
    underlying
  }

  override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)

  def addOne(key: K, value: V): this.type = {
    if (isAliased) {
      underlying = underlying.updated(key, value)
    } else {
      var prev: ListMap.Node[K, V] = null
      var curr = underlying
      while (curr.nonEmpty) {
        if (key == curr.key) {
          if (prev eq null) {
            underlying = underlying.next
          } else {
            prev._init = curr.init
          }
          underlying = new ListMap.Node(curr.key, value, underlying)
          return this
        }
        prev = curr.asInstanceOf[ListMap.Node[K, V]]
        curr = curr.next
      }
      underlying = new ListMap.Node(key, value, underlying)
    }
    this
  }
}
