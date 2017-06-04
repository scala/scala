/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman
package collection
package immutable

import collection.{Iterator, MapFactory}

import scala.annotation.tailrec
import scala.{Any, AnyRef, Array, Boolean, Int, NoSuchElementException, None, Nothing, Option, SerialVersionUID, Serializable, Some, sys}
import java.lang.Integer

import strawman.collection.mutable.Builder

/**
  * This class implements immutable maps using a list-based data structure. List map iterators and
  * traversal methods visit key-value pairs in the order whey were first inserted.
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
  * @version 2.0, 01/01/2007
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(301002838095710379L)
sealed class ListMap[K, +V]
  extends Map[K, V]
     with MapOps[K, V, ListMap, ListMap[K, V]]
     with Serializable {

  def iterableFactory = List

  protected[this] def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): ListMap[K2,V2] = ListMap.fromIterable(it)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[(K, V)]): ListMap[K, V] =
    coll match {
      case lm: ListMap[K, V] => lm
      case _ => ListMap.fromIterable(coll)
    }

  def empty: ListMap[K, V] = ListMap.empty[K, V]

  override def size: Int = 0

  override def isEmpty: Boolean = true

  def get(key: K): Option[V] = None

  def updated[B1 >: V](key: K, value: B1): ListMap[K, B1] = new Node[B1](key, value)

  def remove(key: K): ListMap[K, V] = this

  def iterator(): Iterator[(K, V)] = {
    var curr: ListMap[K, V] = this
    var res: List[(K, V)] = Nil
    while (!curr.isEmpty) {
      res = (curr.key, curr.value) :: res
      curr = curr.next
    }
    res.iterator()
  }

  protected def key: K = throw new NoSuchElementException("key of empty map")
  protected def value: V = throw new NoSuchElementException("value of empty map")
  protected def next: ListMap[K, V] = throw new NoSuchElementException("next of empty map")

  override def className = "ListMap"

  /**
    * Represents an entry in the `ListMap`.
    */
  @SerialVersionUID(-6453056603889598734L)
  protected class Node[V1 >: V](override protected val key: K,
                                override protected val value: V1) extends ListMap[K, V1] with Serializable {

    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[K, V1], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def apply(k: K): V1 = applyInternal(this, k)

    @tailrec private[this] def applyInternal(cur: ListMap[K, V1], k: K): V1 =
      if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
      else if (k == cur.key) cur.value
      else applyInternal(cur.next, k)

    override def get(k: K): Option[V1] = getInternal(this, k)

    @tailrec private[this] def getInternal(cur: ListMap[K, V1], k: K): Option[V1] =
      if (cur.isEmpty) None
      else if (k == cur.key) Some(cur.value)
      else getInternal(cur.next, k)

    override def contains(k: K): Boolean = containsInternal(this, k)

    @tailrec private[this] def containsInternal(cur: ListMap[K, V1], k: K): Boolean =
      if(cur.isEmpty) false
      else if (k == cur.key) true
      else containsInternal(cur.next, k)

    override def updated[V2 >: V1](k: K, v: V2): ListMap[K, V2] = {
      val m = this - k
      new m.Node[V2](k, v)
    }

    override def remove(k: K): ListMap[K, V1] = removeInternal(k, this, Nil)

    @tailrec private[this] def removeInternal(k: K, cur: ListMap[K, V1], acc: List[ListMap[K, V1]]): ListMap[K, V1] =
      if (cur.isEmpty) acc.last
      else if (k == cur.key) acc.foldLeft(cur.next) { (t, h) => new t.Node(h.key, h.value) }
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListMap[K, V1] = ListMap.this

    override def last: (K, V1) = (key, value)
    override def init: ListMap[K, V1] = next
  }
}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list map with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#list_maps "Scala's Collection Library overview"]]
  * section on `List Maps` for more information.
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  */
object ListMap extends MapFactory[ListMap] {

  def empty[K, V]: ListMap[K, V] = EmptyListMap.asInstanceOf[ListMap[K, V]]

  @SerialVersionUID(-8256686706655863282L)
  private object EmptyListMap extends ListMap[Any, Nothing]

  def fromIterable[K, V](it: collection.Iterable[(K, V)]): ListMap[K, V] =
    it match {
      case lm: ListMap[K, V] => lm
      case _ => empty ++ it
    }

}

