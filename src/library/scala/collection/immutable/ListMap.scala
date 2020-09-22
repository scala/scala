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

import generic._
import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

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
object ListMap extends ImmutableMapFactory[ListMap] {

  /**
    * $mapCanBuildFromInfo
    */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, (A, B), ListMap[A, B]]]
  private[this] val ReusableCBF = new MapCanBuildFrom[Any, Any]

  def empty[A, B]: ListMap[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  @SerialVersionUID(-8256686706655863282L)
  private object EmptyListMap extends ListMap[Any, Nothing]

  @tailrec private def foldRightInternal[A, B, Z](map: ListMap[A, B], prevValue: Z, op: ((A, B), Z) => Z): Z = {
    if (map.isEmpty) prevValue
    else foldRightInternal(map.init, op(map.last, prevValue), op)
  }
  /** return the index of the specified key, or -1 if it is not present
   * the index returned in in the nodes internal next based structure so
   * {{{
   *   val x = ListMap( 1 -> a, 2 -> b, 3 -> c)
   *   // so x is Node(key=3, value=c)
   *   indexOf(x, 1) == 2
   *   indexOf(x, 2) == 1
   *   indexOf(x, 3) == 0
   *   indexOf(x, 99) == -1
   * }}}
   *
   */
  @tailrec private def indexOfInternal[A](nodes: ListMap[A, _], key: A, index: Int = 0): Int = {
    if (nodes.isEmpty) -1
    else if (key == nodes.key) index
    else indexOfInternal(nodes.next, key, index + 1)
  }

  /**
   * finds the node with the specified key, or null if it doesnt exist
   */
  @tailrec private def findNodeWithKey[A, B](nodes: ListMap[A, B], key: A): ListMap[A, B] = {
    if (nodes.isEmpty) null
    else if (key == nodes.key) nodes
    else findNodeWithKey(nodes.next, key)
  }
  /**
   * rebuilds a ListMap from nodes. Nodes may contain `null`s which are skipped. Will reuse
   * existing nodes where it can
   */
  private def fromArrayInApiOrder[A, B](nodes: Array[ListMap[A, B]#Node[B]], _tail: ListMap[A, B]): ListMap[A, B] = {
    var tail = _tail
    var index = nodes.length -1
    while (index >= 0) {
      val node = nodes(index)
      if (node ne null)
        if (node.next0 eq tail) tail = node
        else tail = tail.newNode(node.key, node.value)
      index -= 1
    }
    tail
  }

  private def nodesInApiOrder[A, B, B1 >: B](nodes: ListMap[A, B]#Node[B1], len: Int): Array[ListMap[A, B]#Node[B1]] = {
    val result = new Array[ListMap[A, B]#Node[B1]](len)
    result(0) = nodes
    var current = nodes
    var index   = 1
    while (index < len) {
      current = current.next0.asInstanceOf[ListMap[A, B]#Node[B1]]
      result(index) = current
      index += 1
    }

    result
  }
  def removeAtIndex[A, B](map: ListMap[A, B]#Node[B], index: Int): ListMap[A, B] = {
    index match {
      case -1 => map
      case i if i < 8 =>
        def removeAt(node: ListMap[A, B], index: Int): ListMap[A, B] =
          if (index == 0) node.next
          else removeAt(node.next0, index - 1).newNode(node.key, node.value)

        removeAt(map, index)
      case i =>
        val nodes = nodesInApiOrder(map, index)
        val tail = nodes(index-1).next0.next
        fromArrayInApiOrder(nodes, tail)
    }
  }
}

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
  * @tparam A the type of the keys contained in this list map
  * @tparam B the type of the values associated with the keys
  *
  * @author Matthias Zenger
  * @author Martin Odersky
  * @since 1
  * @define Coll ListMap
  * @define coll list map
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(301002838095710379L)
sealed class ListMap[A, +B] extends AbstractMap[A, B]
  with Map[A, B]
  with MapLike[A, B, ListMap[A, B]]
  with Serializable
  with HasForeachEntry[A,B] {

  override def empty = ListMap.empty

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def get(key: A): Option[B] = None

  private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = {
    var current = this
    while (!current.isEmpty) {
      f(current.key, current.value)
      current = current.next
    }
  }

  override def hashCode(): Int = {
    if (isEmpty) {
      MurmurHash3.emptyMapHash
    } else {
      val hasher = new Map.HashCodeAccumulator()
      foreachEntry(hasher)
      hasher.finalizeHash
    }
  }

  override def updated[B1 >: B](key: A, value: B1): ListMap[A, B1] = newNode[B1](key, value)

  def +[B1 >: B](kv: (A, B1)): ListMap[A, B1] = newNode[B1](kv._1, kv._2)
  def -(key: A): ListMap[A, B] = this
  private def newNode[B1 >: B](key: A, value: B1): ListMap[A, B1] = new Node[B1](key, value)

  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): ListMap[A, B1] =
    if (xs.isEmpty) this
    else ((repr: ListMap[A, B1]) /: xs) (_ + _)

  def iterator: Iterator[(A, B)] = {
    def reverseList = {
      var curr: ListMap[A, B] = this
      var res: List[(A, B)] = Nil
      while (!curr.isEmpty) {
        res = (curr.key, curr.value) :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  protected def key: A = throw new NoSuchElementException("key of empty map")
  protected def value: B = throw new NoSuchElementException("value of empty map")
  protected def next: ListMap[A, B] = throw new NoSuchElementException("next of empty map")
  @inline private[ListMap] def next0 = next

  override def foldRight[Z](z: Z)(op: ((A, B), Z) => Z): Z = ListMap.foldRightInternal(this, z, op)
  override def stringPrefix = "ListMap"

  /**
    * Represents an entry in the `ListMap`.
    */
  @SerialVersionUID(-6453056603889598734L)
  protected class Node[B1 >: B](override protected[ListMap] val key: A,
                                override protected[ListMap] val value: B1) extends ListMap[A, B1] with Serializable {

    override def size: Int = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(cur: ListMap[A, B1], acc: Int): Int =
      if (cur.isEmpty) acc
      else sizeInternal(cur.next, acc + 1)

    override def isEmpty: Boolean = false

    override def apply(k: A): B1 = {
      val node = ListMap.findNodeWithKey(this, k)
      if (node eq null) throw new NoSuchElementException("key not found: " + k)
      else node.value
    }

    override def get(k: A): Option[B1] = {
      val node = ListMap.findNodeWithKey(this, k)
      if (node eq null) None
      else Some(node.value)
    }

    override def contains(k: A): Boolean = ListMap.findNodeWithKey(this, k) ne null

    override def updated[B2 >: B1](k: A, v: B2): ListMap[A, B2] =
      // this is incorrect for 2.13 as we need to preserve the keys
      // IN 2.13 - if ((v.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) && (key == k)) this
      if ((v.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) &&
        (k.asInstanceOf[AnyRef] eq key.asInstanceOf[AnyRef])) this
      else {
        // this is incorrect for 2.13 as we need to preserve the keys
        // in 2.13 we lookup k and the map contains oldNode.key -> v
        // in 2.12 we lookup k and the map contains k -> v
        val m = this - k
        new m.Node[B2](k, v)
      }

    override def +[B2 >: B1](kv: (A, B2)): ListMap[A, B2] =
      updated(kv._1, kv._2)

    override def -(k: A): ListMap[A, B1] =
      ListMap.removeAtIndex(this, ListMap.indexOfInternal(this, k))

    override protected def next: ListMap[A, B1] = ListMap.this

    override def last: (A, B1) = (key, value)
    override def init: ListMap[A, B1] = next
  }
}
