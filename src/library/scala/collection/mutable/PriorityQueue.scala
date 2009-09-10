/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.generic.{ Addable, Cloneable, Growable }


/** This class implements priority queues using a heap.
 *  To prioritize elements of type T there must be an implicit
 *  Ordering[T] available at creation.
 *
 *  Martin: This class is utterly broken. It uses a resizable array
 *  as a heap, yet pretends to be a sequence via this same resizable array.
 *  Needless to say, order of elements is different in the two.
 *  So this class needs to be redesigned so that it uses the array only
 *  in its implementation, but implements a sequence interface separately.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 */

@serializable @cloneable
class PriorityQueue[A](implicit ord: Ordering[A])
      extends ResizableArray[A]
      with Addable[A, PriorityQueue[A]]
      with Growable[A]
      with Cloneable[PriorityQueue[A]]
{
  import ord._

  size0 += 1                                    // we do not use array(0)
  override def length: Int = super.length - 1   // adjust length accordingly
  override def isEmpty: Boolean = size0 < 2
  override def repr = this

  // hey foreach, our 0th element doesn't exist
  override def foreach[U](f: A => U) {
    var i = 1
    while (i < size) {
      f(toA(array(i)))
      i += 1
    }
  }

  private def toA(x: AnyRef): A = x.asInstanceOf[A]
  protected def fixUp(as: Array[AnyRef], m: Int): Unit = {
    var k: Int = m
    while (k > 1 && toA(as(k / 2)) < toA(as(k))) {
      swap(k, k / 2)
      k = k / 2
    }
  }
  protected def fixDown(as: Array[AnyRef], m: Int, n: Int): Unit = {
    var k: Int = m
    while (n >= 2 * k) {
      var j = 2 * k
      if (j < n && toA(as(j)) < toA(as(j + 1)))
        j += 1
      if (toA(as(k)) >= toA(as(j)))
        return
      else {
        val h = as(k)
        as(k) = as(j)
        as(j) = h
        k = j
      }
    }
  }

  /** Inserts a single element into the priority queue.
   *
   *  @param  elem        the element to insert
   */
  def +=(elem: A): this.type = {
    ensureSize(size0 + 1)
    array(size0) = elem.asInstanceOf[AnyRef]
    fixUp(array, size0)
    size0 += 1
    this
  }

  def +(elem: A): PriorityQueue[A] = { this.clone() += elem }

  /** Add two or more elements to this set.
   *  @param    elem1 the first element.
   *  @param    kv2 the second element.
   *  @param    kvs the remaining elements.
   */
  override def +(elem1: A, elem2: A, elems: A*) = { this.clone().+=(elem1, elem2, elems : _*) }

  /** Adds all elements provided by an <code>Iterable</code> object
   *  into the priority queue.
   *
   *  @param  iter        an iterable object
   */
  def ++(elems: Traversable[A]) = { this.clone() ++= elems }  // ??? XXX why does this "override nothing" with override?

  /** Adds all elements provided by an iterator into the priority queue.
   *
   *  @param  it        an iterator
   */
  override def ++(iter: Iterator[A]) = { this.clone() ++= iter } // ...whereas this doesn't?

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*): Unit = { this ++= elems }

  /** Returns the element with the highest priority in the queue,
   *  and removes this element from the queue.
   *
   *  @throws Predef.NoSuchElementException
   *  @return   the element with the highest priority.
   */
  def dequeue(): A =
    if (size0 > 1) {
      size0 = size0 - 1
      swap(1, size0)
      fixDown(array, 1, size0 - 1)
      toA(array(size0))
    } else
      throw new NoSuchElementException("no element to remove from heap")

  /** Returns the element with the highest priority in the queue,
   *  or throws an error if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  def max: A = if (size0 > 1) toA(array(1)) else throw new NoSuchElementException("queue is empty")

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  def clear(): Unit = { size0 = 1 }

  /** Returns an iterator which yields all the elements of the priority
   *  queue in descending priority order.
   *
   *  @return  an iterator over all elements sorted in descending order.
   */
  override def iterator: Iterator[A] = new Iterator[A] {
    val as: Array[AnyRef] = new Array[AnyRef](size0)
    Array.copy(array, 0, as, 0, size0)
    var i = size0 - 1
    def hasNext: Boolean = i > 0
    def next(): A = {
      val res = toA(as(1))
      as(1) = as(i)
      i = i - 1
      fixDown(as, 1, i)
      res
    }
  }

  /** This is utterly broken: Two priority queues of different length can still be equal!
   *  The method should be removed once PriotyQueue inserts correctly into the sequence class hierarchy.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: PriorityQueue[_] => (this.iterator zip that.iterator) forall { case (x, y) => x == y }
    case _                      => false
  }

  /** The hashCode method always yields an error, since it is not
   *  safe to use mutable queues as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int =
    throw new UnsupportedOperationException("unsuitable as hash key")

  /** Returns a regular queue containing the same elements.
   */
  def toQueue: Queue[A] = new Queue[A] ++= this.iterator

  /** Returns a textual representation of a queue as a string.
   *
   *  @return the string representation of this queue.
   */
  override def toString() = toList.mkString("PriorityQueue(", ", ", ")")
  override def toList = this.iterator.toList

  /** This method clones the priority queue.
   *
   *  @return  a priority queue with the same elements.
   */
  override def clone(): PriorityQueue[A] = new PriorityQueue[A] ++= this.iterator
}
