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

package scala
package collection
package immutable

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.ReusableBuilder
import scala.collection.generic.DefaultSerializable
import scala.runtime.Statics.releaseFence
import scala.util.hashing.MurmurHash3

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
  * @define Coll ListMap
  * @define coll list map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
sealed class ListMap[K, +V]
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, ListMap, ListMap[K, V]]
    with MapFactoryDefaults[K, V, ListMap, Iterable]
    with DefaultSerializable {

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

  @nowarn("msg=overriding method keys")
  override def keys: Iterable[K] = {
    var curr: ListMap[K, V] = this
    var res: List[K] = Nil
    while (curr.nonEmpty) {
      res = curr.key :: res
      curr = curr.next
    }
    res
  }

  override def hashCode(): Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      // Can't efficiently override foreachEntry directly in ListMap because it would need to preserve iteration
      // order be reversing the list first. But mapHash is symmetric so the reversed order is fine here.
      val _reversed = new immutable.AbstractMap[K, V] {
        override def isEmpty: Boolean = ListMap.this.isEmpty
        override def removed(key: K): Map[K, V] = ListMap.this.removed(key)
        override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = ListMap.this.updated(key, value)
        override def get(key: K): Option[V] = ListMap.this.get(key)
        override def iterator: Iterator[(K, V)] = ListMap.this.iterator
        override def foreachEntry[U](f: (K, V) => U): Unit = {
          var curr: ListMap[K, V] = ListMap.this
          while (curr.nonEmpty) {
            f(curr.key, curr.value)
            curr = curr.next
          }
        }
      }
      MurmurHash3.mapHash(_reversed)
    }
  }

  private[immutable] def key: K = throw new NoSuchElementException("key of empty map")
  private[immutable] def value: V = throw new NoSuchElementException("value of empty map")
  private[immutable] def next: ListMap[K, V] = throw new NoSuchElementException("next of empty map")

  override def foldRight[Z](z: Z)(op: ((K, V), Z) => Z): Z = ListMap.foldRightInternal(this, z, op)
  override protected[this] def className = "ListMap"

}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list map with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#list-maps "Scala's Collection Library overview"]]
  * section on `List Maps` for more information.
  * @define Coll ListMap
  * @define coll list map
  */
@SerialVersionUID(3L)
object ListMap extends MapFactory[ListMap] {
  /**
    * Represents an entry in the `ListMap`.
    */
  private[immutable] final class Node[K, V](
    override private[immutable] val key: K,
    private[immutable] var _value: V,
    private[immutable] var _init: ListMap[K, V]
  ) extends ListMap[K, V] {
    releaseFence()

    override private[immutable] def value: V = _value

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

      var index = -1 // the index (in reverse) where the key to update exists, if it is found
      var found = false // true if the key is found int he map
      var isDifferent = false // true if the key was found and the values are different

      {
        var curr: ListMap[K, V] = this

        while (curr.nonEmpty && !found) {
          if (k == curr.key) {
            found = true
            isDifferent = v.asInstanceOf[AnyRef] ne curr.value.asInstanceOf[AnyRef]
          }
          index += 1
          curr = curr.init
        }
      }

      if (found) {
        if (isDifferent) {
          var newHead: ListMap.Node[K, V1] = null
          var prev: ListMap.Node[K, V1] = null
          var curr: ListMap[K, V1] = this
          var i = 0
          while (i < index) {
            val temp = new ListMap.Node(curr.key, curr.value, null)
            if (prev ne null) {
              prev._init = temp
            }
            prev = temp
            curr = curr.init
            if (newHead eq null) {
              newHead = prev
            }
            i += 1
          }
          val newNode = new ListMap.Node(curr.key, v, curr.init)
          if (prev ne null) {
            prev._init = newNode
          }
          releaseFence()
          if (newHead eq null) newNode else newHead
        } else {
          this
        }
      } else {
        new ListMap.Node(k, v, this)
      }
    }

    @tailrec private[this] def removeInternal(k: K, cur: ListMap[K, V], acc: List[ListMap[K, V]]): ListMap[K, V] =
      if (cur.isEmpty) acc.last
      else if (k == cur.key) acc.foldLeft(cur.next) { (t, h) => new Node(h.key, h.value, t) }
      else removeInternal(k, cur.next, cur :: acc)

    override def removed(k: K): ListMap[K, V] = removeInternal(k, this, Nil)

    override private[immutable] def next: ListMap[K, V] = _init

    override def last: (K, V) = (key, value)
    override def init: ListMap[K, V] = next

  }

  def empty[K, V]: ListMap[K, V] = EmptyListMap.asInstanceOf[ListMap[K, V]]

  private object EmptyListMap extends ListMap[Any, Nothing]

  def from[K, V](it: collection.IterableOnce[(K, V)]): ListMap[K, V] =
    it match {
      case lm: ListMap[K, V] => lm
      case lhm: collection.mutable.LinkedHashMap[K, V] =>
        // by directly iterating through LinkedHashMap entries, we save creating intermediate tuples for each
        // key-value pair
        var current: ListMap[K, V] = empty[K, V]
        var firstEntry = lhm._firstEntry
        while (firstEntry ne null) {
          current = new Node(firstEntry.key, firstEntry.value, current)
          firstEntry = firstEntry.later
        }
        current
      case _: collection.Map[K, V] | _: collection.MapView[K, V] =>
        // when creating from a map, we need not handle duplicate keys, so we can just append each key-value to the end
        var current: ListMap[K, V] = empty[K, V]
        val iter = it.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next()
          current = new Node(k, v, current)
        }
        current

      case _ => (newBuilder[K, V] ++= it).result()
    }

  /** Returns a new ListMap builder
    *
    * The implementation safely handles additions after `result()` without calling `clear()`
    *
    * @tparam K the map key type
    * @tparam V the map value type
    */
  def newBuilder[K, V]: ReusableBuilder[(K, V), ListMap[K, V]] = new ListMapBuilder[K, V]

  @tailrec private def foldRightInternal[K, V, Z](map: ListMap[K, V], prevValue: Z, op: ((K, V), Z) => Z): Z = {
    if (map.isEmpty) prevValue
    else foldRightInternal(map.init, op(map.last, prevValue), op)
  }
}

/** Builder for ListMap.
  * $multipleResults
  */
private[immutable] final class ListMapBuilder[K, V] extends mutable.ReusableBuilder[(K, V), ListMap[K, V]] {
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

  @tailrec
  private[this] def insertValueAtKeyReturnFound(m: ListMap[K, V], key: K, value: V): Boolean = m match {
    case n: ListMap.Node[K, V] =>
      if (n.key == key) {
        n._value = value
        true
      } else {
        insertValueAtKeyReturnFound(n.init, key, value)
      }
    case _ => false
  }

  def addOne(key: K, value: V): this.type = {
    if (isAliased) {
      underlying = underlying.updated(key, value)
    } else {
      if (!insertValueAtKeyReturnFound(underlying, key, value)) {
        underlying = new ListMap.Node(key, value, underlying)
      }
    }
    this
  }
  override def addAll(xs: IterableOnce[(K, V)]): this.type = {
    if (isAliased) {
      super.addAll(xs)
    } else if (underlying.nonEmpty) {
      xs match {
        case m: collection.Map[K, V] =>
          // if it is a map, then its keys will not collide with themselves.
          // therefor we only need to check the already-existing elements for collisions.
          // No need to check the entire list

          val iter = m.iterator
          var newUnderlying = underlying
          while (iter.hasNext) {
            val next = iter.next()
            if (!insertValueAtKeyReturnFound(underlying, next._1, next._2)) {
              newUnderlying = new ListMap.Node[K, V](next._1, next._2, newUnderlying)
            }
          }
          underlying = newUnderlying
          this

        case _ =>
          super.addAll(xs)
      }
    } else xs match {
      case lhm: collection.mutable.LinkedHashMap[K, V] =>
        // special-casing LinkedHashMap avoids creating of Iterator and tuples for each key-value
        var firstEntry = lhm._firstEntry
        while (firstEntry ne null) {
          underlying = new ListMap.Node(firstEntry.key, firstEntry.value, underlying)
          firstEntry = firstEntry.later
        }
        this

      case _: collection.Map[K, V] | _: collection.MapView[K, V] =>
        val iter = xs.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next()
          underlying = new ListMap.Node(k, v, underlying)
        }

        this
      case _ =>
        super.addAll(xs)
    }
  }
}
