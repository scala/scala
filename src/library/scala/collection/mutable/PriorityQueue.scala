/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


//import Predef.{NoSuchElementException, UnsupportedOperationException}

/** This class implements priority queues using a heap. The
 *  elements of the queue have to be ordered in terms of the
 *  <code>Ordered[T]</code> class.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 */

@serializable @cloneable
class PriorityQueue[A <% Ordered[A]] extends ResizableArray[A] {
  size = size + 1 // we do not use array(0)

  protected def fixUp(as: Array[A], m: Int): Unit = {
    var k: Int = m
    while ((k > 1) && (as(k / 2) < as(k))) {
      swap(k, k / 2)
      k = k / 2
    }
  }

  protected def fixDown(as: Array[A], m: Int, n: Int): Unit = {
    var k: Int = m
    var loop: Boolean = true
    while (loop && (n >= 2 * k)) {
      var j = 2 * k
      if ((j < n) && (as(j) < as(j + 1)))
        j = j + 1;
      if (!(as(k) < as(j)))
        loop = false
      else {
        val h = as(k)
        as(k) = as(j)
        as(j) = h
        k = j
      }
    }
  }

  /** Checks if the queue is empty.
   *
   *  @return true, iff there is no element in the queue.
   */
  override def isEmpty: Boolean = size < 2

  /** Inserts a single element into the priority queue.
   *
   *  @param  elem        the element to insert
   */
  def +=(elem: A): Unit = {
    ensureSize(size+1)
    array(size) = elem
    fixUp(array, size)
    size = size + 1
  }

  def +(elem: A): PriorityQueue[A] = { this += elem; this }

  /** Add two or more elements to this set.
   *  @param    elem1 the first element.
   *  @param    kv2 the second element.
   *  @param    kvs the remaining elements.
   */
  def += (elem1: A, elem2: A, elems: A*) { this += elem1; this += elem2; this ++= elems }

  def + (elem1: A, elem2: A, elems: A*) = { this.+=(elem1, elem2, elems: _*); this }

  /** Adds all elements provided by an <code>Iterable</code> object
   *  into the priority queue.
   *
   *  @param  iter        an iterable object
   */
  def ++=(iter: Iterable[A]): Unit = this ++= iter.elements

  /** Adds all elements provided by an iterator into the priority queue.
   *
   *  @param  it        an iterator
   */
  def ++=(it: Iterator[A]): Unit = it foreach { e => this += e }

  def ++(iter: Iterable[A]): PriorityQueue[A] = { this ++= iter; this }

  def ++(iter: Iterator[A]): PriorityQueue[A] = { this ++= iter; this }

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*): Unit = (this ++= elems)

  /** Returns the element with the highest priority in the queue,
   *  and removes this element from the queue.
   *
   *  @throws Predef.NoSuchElementException
   *  @return   the element with the highest priority.
   */
  def dequeue(): A =
    if (size > 1) {
      size = size - 1
      swap(1, size)
      fixDown(array, 1, size - 1)
      array(size)
    } else
      throw new NoSuchElementException("no element to remove from heap")

  /** Returns the element with the highest priority in the queue,
   *  or throws an error if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  def max: A = if (size > 1) array(1) else throw new NoSuchElementException("queue is empty")

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  def clear(): Unit = { size = 1 }

  /** Returns an iterator which yiels all the elements of the priority
   *  queue in descending priority order.
   *
   *  @return  an iterator over all elements sorted in descending order.
   */
  override def elements: Iterator[A] = new Iterator[A] {
    val as: Array[A] = new Array[A](size)
    Array.copy(array, 0, as, 0, size)
    var i = size - 1
    def hasNext: Boolean = i > 0
    def next(): A = {
      val res = as(1)
      as(1) = as(i)
      i = i - 1
      fixDown(as, 1, i)
      res
    }
  }

  /** Checks if two queues are structurally identical.
   *
   *  @return true, iff both queues contain the same sequence of elements.
   */
  override def equals(that: Any): Boolean =
    that.isInstanceOf[PriorityQueue[A]] &&
    { val other = that.asInstanceOf[PriorityQueue[A]]
      elements.zip(other.elements).forall {
        case (thiselem, thatelem) => thiselem == thatelem
    }}

  /** The hashCode method always yields an error, since it is not
   *  safe to use mutable queues as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int =
    throw new UnsupportedOperationException("unsuitable as hash key")

  /** Returns a regular queue containing the same elements.
   */
  def toQueue: Queue[A] = {
    val res = new Queue[A]
    res ++= this
    res
  }

  /** Returns a textual representation of a queue as a string.
   *
   *  @return the string representation of this queue.
   */
  override def toString() = toList.mkString("PriorityQueue(", ", ", ")")

  /** This method clones the priority queue.
   *
   *  @return  a priority queue with the same elements.
   */
  override def clone(): PriorityQueue[A] = {
    val res = new PriorityQueue[A]
    res ++= this
    res
  }
}
