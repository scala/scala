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

import mutable.{Builder, ImmutableBuilder}
import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable

/**
  * This class implements immutable sets using a list-based data structure. List set iterators and
  * traversal methods visit elements in the order they were first inserted.
  *
  * Elements are stored internally in reversed insertion order, which means the newest element is at
  * the head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and
  * `init` are O(1). Other operations, such as inserting or removing entries, are also O(n), which
  * makes this collection suitable only for a small number of elements.
  *
  * Instances of `ListSet` represent empty sets; they can be either created by calling the
  * constructor directly, or by applying the function `ListSet.empty`.
  *
  * @tparam A the type of the elements contained in this list set
  *
  * @define Coll ListSet
  * @define coll list set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
sealed class ListSet[A]
  extends AbstractSet[A]
    with StrictOptimizedSetOps[A, ListSet, ListSet[A]]
    with IterableFactoryDefaults[A, ListSet]
    with DefaultSerializable {

  override protected[this] def className: String = "ListSet"

  override def size: Int = 0
  override def knownSize: Int = 0
  override def isEmpty: Boolean = true

  def contains(elem: A): Boolean = false

  def incl(elem: A): ListSet[A] = new Node(elem)
  def excl(elem: A): ListSet[A] = this

  def iterator: scala.collection.Iterator[A] = {
    var curr: ListSet[A] = this
    var res: List[A] = Nil
    while (!curr.isEmpty) {
      res = curr.elem :: res
      curr = curr.next
    }
    res.iterator
  }

  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  protected def next: ListSet[A] = throw new NoSuchElementException("next of empty set")

  override def iterableFactory: IterableFactory[ListSet] = ListSet

  /**
    * Represents an entry in the `ListSet`.
    */
  protected class Node(override protected val elem: A) extends ListSet[A] {

    override def size = sizeInternal(this, 0)
    override def knownSize: Int = -1
    @tailrec private[this] def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A): Boolean = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: ListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.next, e))

    override def incl(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    override def excl(e: A): ListSet[A] = removeInternal(e, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: ListSet[A], acc: List[ListSet[A]]): ListSet[A] =
      if (cur.isEmpty) acc.last
      else if (k == cur.elem) acc.foldLeft(cur.next)((t, h) => new t.Node(h.elem))
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListSet[A] = ListSet.this

    override def last: A = elem

    override def init: ListSet[A] = next
  }
}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @define Coll ListSet
  * @define coll list set
  */
@SerialVersionUID(3L)
object ListSet extends IterableFactory[ListSet] {

  def from[E](it: scala.collection.IterableOnce[E]): ListSet[E] =
    it match {
      case ls: ListSet[E] => ls
      case _ if it.knownSize == 0 => empty[E]
      case _ => (newBuilder[E] ++= it).result()
    }

  private object EmptyListSet extends ListSet[Any] {
    override def knownSize: Int = 0
  }
  private[collection] def emptyInstance: ListSet[Any] = EmptyListSet

  def empty[A]: ListSet[A] = EmptyListSet.asInstanceOf[ListSet[A]]

  def newBuilder[A]: Builder[A, ListSet[A]] =
    new ImmutableBuilder[A, ListSet[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems + elem; this }
    }
}
