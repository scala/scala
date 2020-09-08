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

  private def fillArray[A, B](from: ListMap[A, B]): Array[ListMap[A, _]#Node[B]] = {
    fillArraySized(from, from.size)
  }
  private def fillArraySized[A, B](from: ListMap[A, B], size: Int): Array[ListMap[A, _]#Node[B]] = {
    val array = new Array[ListMap[A, _]#Node[B]](size)
    fillArrayFromMap(from, array, 0)
    array
  }
  @tailrec private def fillArrayFromMap[A, B](from: ListMap[A, B], to: Array[ListMap[A, _]#Node[B]], start: Int): ListMap[A, B] =
    if (to.length <= start) from.next0
    else {
      to(start) = from.asInstanceOf[ListMap[A, _]#Node[B]]
      fillArrayFromMap(from.next0, to, start + 1)
    }
  @tailrec final def dropInternal[A, B](cur: ListMap[A, B], n: Int): ListMap[A, B] = {
    if (n <= 0 || cur.isEmpty) cur
    else dropInternal(cur.next, n - 1)
  }
  @tailrec private def getAtIndex[A, B](cur: ListMap[A, B], index: Int): ListMap[A, B] =
    if (index == 0) cur
    else getAtIndex(cur.next, index -1)

  private def findIndexInternal[A, B](cur: ListMap[A, B], key: A): Int =
    findIndexInternal0(cur, key, 0)
  @tailrec private[this] def findIndexInternal0[A, B](cur: ListMap[A, B], key: A, currentIndex: Int): Int =
    if (cur.isEmpty) -1
    else if (key == cur.key) currentIndex
    else findIndexInternal0(cur.next, key, currentIndex + 1)
  /** get the node with the matching key, or null */
  @tailrec private def findNodeInternal[A, B](cur: ListMap[A, B], key: A): ListMap[A, B] =
    if (cur.isEmpty) null
    else if (key == cur.key) cur
    else findNodeInternal(cur.next, key)

  @tailrec private def sizeInternal[A, B](cur: ListMap[A, B], acc: Int): Int =
    if (cur.isEmpty) acc
    else sizeInternal(cur.next, acc + 1)


  @tailrec private def applyInternal[A, B](cur: ListMap[A, B], k: A): B =
    if (cur.isEmpty) throw new NoSuchElementException("key not found: " + k)
    else if (k == cur.key) cur.value
         else applyInternal(cur.next, k)


  @tailrec private def getInternal[A, B](cur: ListMap[A, B], k: A): Option[B] =
    if (cur.isEmpty) None
    else if (k == cur.key) Some(cur.value)
         else getInternal(cur.next, k)

  /** adds nodes from an array to an existing tail*/
  def addNodes[A, B]( from: Array[ListMap[A, _]#Node[B]], tail: ListMap[A, B]): ListMap[A, B] =
    makeNodes(0, from, tail)
  @tailrec private[this]def makeNodes[A, B](index:Int, fromArray: Array[ListMap[A, _]#Node[B]], tail: ListMap[A, B]): ListMap[A, B] =
    if (index < 0) tail
    else {
      val fromNode = fromArray(index)
      makeNodes(index -1, fromArray, tail.newNode(fromNode.key0, fromNode.value0))
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

  override def apply(key: A): B = default(key)
  override def get(key: A): Option[B] = None
  override def getOrElse[V1 >: B](key: A, default: => V1): V1 = default
  override def applyOrElse[K1 <: A, V1 >: B](key: K1, default: K1 => V1): V1 = default(key)

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

  override def updated[B1 >: B](key: A, value: B1): ListMap[A, B1] = new Node[B1](key, value)

  def +[B1 >: B](kv: (A, B1)): ListMap[A, B1] = new Node[B1](kv._1, kv._2)
  def -(key: A): ListMap[A, B] = this

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
  @inline private[ListMap] final def next0 = next
  @inline private[ListMap] final def key0 = key
  @inline private[ListMap] final def value0 = value

  override def foldRight[Z](z: Z)(op: ((A, B), Z) => Z): Z = ListMap.foldRightInternal(this, z, op)
  override def stringPrefix = "ListMap"
  private[immutable] def newNode[B1 >: B](k: A, v: B1): Node[B1] = new Node(k, v)
  override def transform[W, That](f: (A, B) => W)(implicit bf: CanBuildFrom[ListMap[A, B], (A, W), That]): That = {
    if (bf eq ListMap.canBuildFrom[A, B]) (this match {
      case nonEmpty: Node[B] => nonEmpty.transformNonEmpty(f)
      case _                 => empty
    }).asInstanceOf[That]
    else super.transform(f)(bf)
  }

  /**
    * Represents an entry in the `ListMap`.
    */
  @SerialVersionUID(-6453056603889598734L)
  protected final class Node[B1 >: B](override protected val key: A,
                                      override protected val value: B1)
    extends ListMap[A, B1]
      with NonEmptyListMapForHashCollision[A, B1]
      with Serializable {

    override def size: Int = ListMap.sizeInternal(this, 0)

    override def isEmpty: Boolean = false

    override def apply(key: A): B1 = {
      val node = ListMap.findNodeInternal(this, key)
      if (node eq null) throw new NoSuchElementException("key not found: " + key)
      else node.value
    }
    override def applyOrElse[K1 <: A, V1 >: B1](key: K1, default: K1 => V1): V1 = {
      val node = ListMap.findNodeInternal(this, key)
      if (node eq null) default(key)
      else node.value
    }
    override def get(key: A): Option[B1] = {
      val node = ListMap.findNodeInternal(this, key)
      if (node eq null) None
      else Some(node.value)
    }
    override def getOrElse[V1 >: B1](key: A, default: => V1): V1 = {
      val node = ListMap.findNodeInternal(this, key)
      if (node eq null) default
      else node.value
    }

    override def contains(k: A): Boolean = ListMap.findNodeInternal(this, k) != null

    override def updated[B2 >: B1](k: A, v: B2): ListMap[A, B2] =  {
      val atIndex = ListMap.findIndexInternal(this, k)
      if (atIndex == -1) newNode[B2](k, v)
      else if (atIndex == 0 && (value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef]))
        this
      else removeAtIndex(atIndex, true).newNode[B2](k, v)
    }

    override def head: (A, B1) = (key, value)
    override def headOption: Option[(A, B1)] = Some(head)
    override def tail: ListMap[A, B1] = drop(1)
    override def last: (A, B1) = last0(this)
    override def lastOption: Option[(A, B1)] = Some(last)
    private[this] def last0(cur: ListMap[A,B1]): (A, B1) = {
      val n = next
      if (n.isEmpty) (cur.key, cur.value)
      else last0(n)
    }
    override def init: ListMap[A, B1] =
      if (next.isEmpty) this
      else empty.newNode(key, value)

    override def drop(n: Int): ListMap[A, B1] = ListMap.dropInternal(this, n)

    override def +[B2 >: B1](kv: (A, B2)): ListMap[A, B2] = updated(kv._1, kv._2)

    override def -(k: A): ListMap[A, B1] = removeAtIndex(ListMap.findIndexInternal(this, k), true)

    /**
     * remove a value from the ListMap
     * @param index the index to remove. Must be <= the size
     * @param retainOrder is strict order required
     * @return
     */
    private[this] def removeAtIndex(index: Int, retainOrder: Boolean): ListMap[A, B1] = {
      index match {
        case -1 => this
        case 0 => next
        case 1 =>
          val n0 = this
          val n2 = next.next
          n2.newNode(n0.key, n0.value)
        case 2 =>
          val n0 = this
          val n1 = n0.next
          val n3 = n1.next.next
          val new1 = n3.newNode(n1.key, n1.value)
          val new0 = new1.newNode(n0.key, n0.value)
          new0
        case 3 =>
          val n0 = this
          val n1 = n0.next
          val n2 = n1.next
          val n4 = n2.next.next
          val new2 = n4.newNode(n2.key, n2.value)
          val new1 = new2.newNode(n1.key, n1.value)
          val new0 = new1.newNode(n0.key, n0.value)
          new0
        case 4 =>
          val n0 = this
          val n1 = n0.next
          val n2 = n1.next
          val n3 = n2.next
          val n5 = n3.next.next
          val new3 = n5.newNode(n3.key, n3.value)
          val new2 = new3.newNode(n2.key, n2.value)
          val new1 = new2.newNode(n1.key, n1.value)
          val new0 = new1.newNode(n0.key, n0.value)
          new0
        case _ if !retainOrder =>
          @tailrec def makeNodes(index:Int, cur: ListMap[A, B1], tail: ListMap[A, B1]): ListMap[A, B1] =
            if (index < 0) tail
            else {
              val from = cur.asInstanceOf[Node[B1]]
              makeNodes(index -1, cur.next, tail.newNode(from.key, from.value))
            }
          makeNodes(index, this, drop(index + 1))
        case _ =>
          val nodes = ListMap.fillArraySized(this, index-1)

          @tailrec def makeNodes(index:Int, tail: ListMap[A, B1]): ListMap[A, B1] =
            if (index < 0) tail
            else {
              val from = nodes(index)
              makeNodes(index - 1, tail.newNode(from.key, from.value))
            }
          makeNodes(index - 1, nodes(nodes.length-1).next)
      }
    }


    override protected def next: ListMap[A, B1] = ListMap.this


    // NonEmptyListMapForHashCollision methods

    def removeUnordered(k: A): ListMap[A, B1] = {
      removeAtIndex(ListMap.findIndexInternal(this, k), true)
    }

    def updatedUnordered[B2 >: B1](k: A, v: B2, kvOrNull: (A, B2), merger: HashMap.Merger[A, B2]): NonEmptyListMapForHashCollision[A, B2] = {
      val atIndex = ListMap.findIndexInternal(this, k)
      if (atIndex == -1) new Node[B2](k, v)
      else if (atIndex == 0 && ((merger eq null) || merger.retainIdentical) && (value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef]))
        this.asInstanceOf[NonEmptyListMapForHashCollision[A, B2]]
      else {
        val node = ListMap.getAtIndex(this, atIndex)
        if (merger eq null)
          removeAtIndex(atIndex, false).newNode[B2](node.key:A, v:B2)
        else if (merger eq HashMap.defaultMerger)
          //prefer the existing
          this
        else if (merger eq HashMap.defaultMerger.invert)
          //prefer the new k/v
          if ((node.value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef]) &&
             (node.key.asInstanceOf[AnyRef] eq k.asInstanceOf[AnyRef]))
            this
          else removeAtIndex(atIndex, false).newNode[B2](k, v)
        else if (merger eq HashMap.concatMerger)
          // prefer existing key and new value
          if (node.value.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef])
            this
          else removeAtIndex(atIndex, false).newNode[B2](node.key, v)
        else if (merger eq HashMap.concatMerger.invert)
           //prefer the new k and old value
           if (node.key.asInstanceOf[AnyRef] eq k.asInstanceOf[AnyRef])
             this
           else removeAtIndex(atIndex, false).newNode[B2](k, node.value)
        else {
          val kv = if (kvOrNull eq null) (k,v) else kvOrNull
          val merged = merger((node.key, node.value), kv)
          if ((merged._1.asInstanceOf[AnyRef] eq node.key.asInstanceOf[AnyRef]) &&
            (merged._2.asInstanceOf[AnyRef] eq node.value.asInstanceOf[AnyRef]))
            this
          else removeAtIndex(atIndex, false).newNode[B2](merged._1, merged._2)
        }
      }
    }
    def filterUnordered(p: ((A, B1)) => Boolean, negate: Boolean): ListMap[A, B1] = {
???
    }

    def merge0[B2 >: B1](kvs: NonEmptyListMapForHashCollision[A, B2], merger: HashMap.Merger[A, B2]): Node[B2] = ???

    override def transformNonEmpty[W](f: (A, B1) => W): NonEmptyListMapForHashCollision[A, W] = {
      val size = this.size
      val result: NonEmptyListMapForHashCollision[A, W] = if (size < 16) {
        // for small maps we can avoid the allocation of array
        def transform0(cur: NonEmptyListMapForHashCollision[A, B1]): NonEmptyListMapForHashCollision[A, W] = {
          val resultTail  = if (cur.next.isEmpty) empty else transform0(cur.next.asInstanceOf[Node[B1]])
          val transformed = f(key, value)
          if ((resultTail eq cur.next) && (transformed.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]))
            cur.asInstanceOf[NonEmptyListMapForHashCollision[A, W]]
          else resultTail.newNode(key, transformed)
        }
        transform0(this)
      } else {
        val nodes = ListMap.fillArray(this)
        var counter = nodes.length -1
        var tail: NonEmptyListMapForHashCollision[A, W] = null
        // work backward through the array transforming as we go, but preserving the node if the
        // transform doesn't change the value, and the tail is unchanged
        while (counter >= 0) {
          val node = nodes(counter)
          val newValue = f(node.key, node.value)
          tail =
            if (tail eq null) empty.newNode(node.key, newValue)
            else if ((tail eq node.next) && (node.value.asInstanceOf[AnyRef] eq newValue.asInstanceOf[AnyRef]))
             node.asInstanceOf[NonEmptyListMapForHashCollision[A, W]]
          else tail.newNode(node.key, newValue)
          counter -= 1
        }
        tail
      }
      //assert (result.size == size)
      result
    }

  }
}
private[immutable] sealed trait NonEmptyListMapForHashCollision[A, +B1] extends ListMap[A, B1] {

  /**
   * removes the key.
   * If the key is not present, returns this
   * if the key is present the ordering of the returned ListMap may be different
   */
  private[immutable] def removeUnordered(key: A): ListMap[A, B1]

  /**
   * filters this.
   * If the no values are removed, returns this
   * if not the ordering of the returned ListMap may be different
   */
  private[immutable] def filterUnordered(p: ((A, B1)) => Boolean, negate: Boolean): ListMap[A, B1]

  /**
   * updates this with the new key and value, using the merge semantics from HashMap.
   * If the result contains the the same key -> value mapping, returns this
   * if not the ordering of the returned ListMap may be different
   */
  private[immutable] def updatedUnordered[B2 >: B1](key: A, value: B2, kvOrNull: (A, B2), merger: HashMap.Merger[A, B2]): NonEmptyListMapForHashCollision[A, B2]

  /**
   * merge this with the kvs, using the merge semantics from HashMap.
   * If the result contains the the same key -> value mapping as this, returns this
   * If the result contains the the same key -> value mapping as kvs, returns kvs
   * if not the ordering of the returned collection is undefined
   */
  private[immutable] def merge0[B2 >: B1](kvs: NonEmptyListMapForHashCollision[A, B2], merger: HashMap.Merger[A, B2]): NonEmptyListMapForHashCollision[A, B2]

  /**
   * transform the values.
   * If the result contains the the same key -> value mapping as this, returns this
   * if not the ordering of the returned collection is undefined
   */
  private[immutable] def transformNonEmpty[W](f: (A, B1) => W): NonEmptyListMapForHashCollision[A, W]

}