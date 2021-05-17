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

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @since 1
  * @define Coll ListSet
  * @define coll list set
  */
object ListSet extends ImmutableSetFactory[ListSet] {

  /**
    * $setCanBuildFromInfo
    */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ListSet[A]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, A, ListSet[A]]]
  private[this] val ReusableCBF = setCanBuildFrom[Any]

  @SerialVersionUID(5010379588739277132L)
  private object EmptyListSet extends ListSet[Any]
  private[collection] def emptyInstance: ListSet[Any] = EmptyListSet
}

/**
  * This class implements immutable sets using a list-based data structure. List set iterators and
  * traversal methods visit elements in the order whey were first inserted.
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
  * @author Matthias Zenger
  * @since 1
  * @define Coll ListSet
  * @define coll list set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
@SerialVersionUID(-8417059026623606218L)
sealed class ListSet[A] extends AbstractSet[A]
  with Set[A]
  with GenericSetTemplate[A, ListSet]
  with SetLike[A, ListSet[A]]
  with Serializable {

  override def companion: GenericCompanion[ListSet] = ListSet

  override def size: Int = 0
  override def isEmpty: Boolean = true

  def contains(elem: A): Boolean = false

  def +(elem: A): ListSet[A] = new Node(elem)
  def -(elem: A): ListSet[A] = this

  override def ++(xs: GenTraversableOnce[A]): ListSet[A] =
    xs match {
      // we want to avoid to use of iterator as it causes allocations
      // during reverseList
      case ls: ListSet[A] =>
        if (ls eq this) this
        else {
          val lsSize = ls.size
          if (lsSize == 0) this
          else if (isEmpty) ls
          else {
            @tailrec def skip(ls: ListSet[A], count: Int): ListSet[A] = {
              if (count == 0) ls else skip(ls.next, count - 1)
            }

            @tailrec def containsLimited(n: ListSet[A], e: A, end: ListSet[A]): Boolean =
              (n ne end) && (e == n.elem || containsLimited(n.next, e, end))

            @tailrec def distanceTo(n: ListSet[A], end: ListSet[A], soFar: Int): Int =
              if (n eq end) soFar else distanceTo(n.next, end, soFar + 1)

            // We hope to get some structural sharing so find the tail of the
            // ListSet that are `eq` (or if there are not any then the ends of the lists),
            // and we optimise the add to only iterate until we reach the common end
            val thisSize  = this.size
            val remaining = Math.min(thisSize, lsSize)
            var thisTail  = skip(this, thisSize - remaining)
            var lsTail    = skip(ls, lsSize - remaining)
            //find out what part of the the ListSet is sharable
            //as we can ignore the shared elements
            while ((thisTail ne lsTail) && !lsTail.isEmpty) {
              thisTail = thisTail.next
              lsTail = lsTail.next
            }
            var toAdd              = ls
            var result: ListSet[A] = this

            // Its quite a common case that we are just adding a few elements, so it there are less than 5 elements we
            // hold them in pending0..3
            // if there are more than these 4 we hold the rest in pending
            var pending                               : Array[A] = null
            var pending0, pending1, pending2, pending3: A        = null.asInstanceOf[A]
            var pendingCount                                     = 0
            while (toAdd ne lsTail) {
              val elem = toAdd.elem
              if (!containsLimited(result, elem, lsTail)) {
                pendingCount match {
                  case 0 => pending0 = elem
                  case 1 => pending1 = elem
                  case 2 => pending2 = elem
                  case 3 => pending3 = elem
                  case _ =>
                    if (pending eq null)
                      pending = new Array[AnyRef](distanceTo(toAdd, lsTail, 0)).asInstanceOf[Array[A]]
                    pending(pendingCount - 4) = elem
                }
                pendingCount += 1
              }
              toAdd = toAdd.next
            }
            // add the extra values. They are added in reverse order so as to ensure that the iteration order is correct
            // remembering that the content is in the reverse order to the iteration order
            // i.e. this.next is really the previous value
            while (pendingCount > 0) {
              val elem: A = pendingCount match {
                case 1 => pending0
                case 2 => pending1
                case 3 => pending2
                case 4 => pending3
                case _ => pending(pendingCount - 5)
              }
              val r       = result
              result = new r.Node(elem)
              pendingCount -= 1
            }
            result
          }
        }
      case _              =>
        if (xs.isEmpty) this
        else (repr /: xs) (_ + _)
    }

  def iterator: Iterator[A] = {
    def reverseList = {
      var curr: ListSet[A] = this
      var res: List[A] = Nil
      while (!curr.isEmpty) {
        res = curr.elem :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  protected def next: ListSet[A] = throw new NoSuchElementException("next of empty set")

  override def toSet[B >: A]: Set[B] = this.asInstanceOf[ListSet[B]]

  override def stringPrefix = "ListSet"

  /**
    * Represents an entry in the `ListSet`.
    */
  @SerialVersionUID(-787710309854855049L)
  protected class Node(override protected val elem: A) extends ListSet[A] with Serializable {

    override def size = sizeInternal(this, 0)

    @tailrec private[this] def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A) = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: ListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.next, e))

    @tailrec private[this] def indexInternal(n: ListSet[A], e: A, i:Int): Int =
      if (n.isEmpty) -1
      else if (n.elem == e) i
      else indexInternal(n.next, e, i + 1)

    override def +(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    override def -(e: A): ListSet[A] = {
      val index = indexInternal(this, e, 0)
      if (index < 0) this
      else if (index == 0) next
      else {
        val data = new Array[ListSet[A]](index)
        @tailrec def store(i: Int, e: ListSet[A]): Unit = {
          if (i < index) {
            data(i) = e
            store(i + 1, e.next)
          }
        }
        @tailrec def reform(i: Int, e: ListSet[A]): ListSet[A] = {
          if (i < 0) e
          else reform (i -1, new e.Node(data(i).elem))
        }
        store(0, this)
        reform(index -1, data(index - 1).next.next)
      }
    }

    override protected def next: ListSet[A] = ListSet.this

    override def last: A = elem
    override def init: ListSet[A] = next
  }
}
