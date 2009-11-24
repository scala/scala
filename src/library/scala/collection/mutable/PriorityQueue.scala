/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._


/** This class implements priority queues using a heap.
 *  To prioritize elements of type T there must be an implicit
 *  Ordering[T] available at creation.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 */

@serializable @cloneable
class PriorityQueue[A](implicit ord: Ordering[A])
      extends Seq[A]
      with SeqLike[A, PriorityQueue[A]]
      with Addable[A, PriorityQueue[A]]
      with Growable[A]
      with Cloneable[PriorityQueue[A]]
      with Builder[A, PriorityQueue[A]]
{
  import ord._

  private class ResizableArrayAccess[A] extends ResizableArray[A] {
    @inline def p_size0 = size0
    @inline def p_size0_=(s: Int) = size0 = s
    @inline def p_array = array
    @inline def p_ensureSize(n: Int) = super.ensureSize(n)
    @inline def p_swap(a: Int, b: Int) = super.swap(a, b)
  }

  protected[this] override def newBuilder = new PriorityQueue[A]

  private val resarr = new ResizableArrayAccess[A]

  resarr.p_size0 += 1                                    // we do not use array(0)
  override def length: Int = resarr.length - 1   // adjust length accordingly
  override def size: Int = length
  override def isEmpty: Boolean = resarr.p_size0 < 2
  override def repr = this

  // hey foreach, our 0th element doesn't exist
  override def foreach[U](f: A => U) {
    var i = 1
    while (i < resarr.p_size0) {
      f(toA(resarr.p_array(i)))
      i += 1
    }
  }

  def update(idx: Int, elem: A) {
    if (idx < 0 || idx >= size) throw new IndexOutOfBoundsException("Indices must be nonnegative and lesser than the size.")

    var i = 0
    val iter = iterator
    clear
    while (iter.hasNext) {
      val curr = iter.next
      if (i == idx) this += elem
      else this += curr
      i += 1
    }
  }

  def apply(idx: Int) = {
    if (idx < 0 || idx >= size) throw new IndexOutOfBoundsException("Indices must be nonnegative and lesser than the size.")

    var left = idx
    val iter = iterator
    var curr = iter.next
    while (left > 0) {
      curr = iter.next
      left -= 1
    }
    curr
  }

  def result = clone

  private def toA(x: AnyRef): A = x.asInstanceOf[A]
  protected def fixUp(as: Array[AnyRef], m: Int): Unit = {
    var k: Int = m
    while (k > 1 && toA(as(k / 2)) < toA(as(k))) {
      resarr.p_swap(k, k / 2)
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
    resarr.p_ensureSize(resarr.p_size0 + 1)
    resarr.p_array(resarr.p_size0) = elem.asInstanceOf[AnyRef]
    fixUp(resarr.p_array, resarr.p_size0)
    resarr.p_size0 += 1
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
  override def ++(elems: scala.collection.Traversable[A]) = { this.clone() ++= elems }

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
    if (resarr.p_size0 > 1) {
      resarr.p_size0 = resarr.p_size0 - 1
      resarr.p_swap(1, resarr.p_size0)
      fixDown(resarr.p_array, 1, resarr.p_size0 - 1)
      toA(resarr.p_array(resarr.p_size0))
    } else
      throw new NoSuchElementException("no element to remove from heap")

  /** Returns the element with the highest priority in the queue,
   *  or throws an error if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  def max: A = if (resarr.p_size0 > 1) toA(resarr.p_array(1)) else throw new NoSuchElementException("queue is empty")

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  def clear(): Unit = { resarr.p_size0 = 1 }

  /** Returns an iterator which yields all the elements of the priority
   *  queue in descending priority order.
   *
   *  @return  an iterator over all elements sorted in descending order.
   */
  override def iterator: Iterator[A] = new Iterator[A] {
    val as: Array[AnyRef] = new Array[AnyRef](resarr.p_size0)
    Array.copy(resarr.p_array, 0, as, 0, resarr.p_size0)
    var i = resarr.p_size0 - 1
    def hasNext: Boolean = i > 0
    def next(): A = {
      val res = toA(as(1))
      as(1) = as(i)
      i = i - 1
      fixDown(as, 1, i)
      res
    }
  }

  /**
   * Returns the reverse of this queue. The priority queue that gets
   * returned will have an inversed ordering - if for some elements
   * <code>x</code> and <code>y</code> the original queue's ordering
   * had <code>compare</code> returning an integer w, the new one will return -w,
   * assuming the original ordering abides its contract.
   *
   * Note that the order of the elements will be reversed unless the
   * <code>compare</code> method returns 0. In this case, such elements
   * will be subsequent, but their corresponding subinterval may be inappropriately
   * reversed. However, due to the compare-equals contract, they will also be equal.
   */
  override def reverse = {
    val revq = new PriorityQueue[A]()(new math.Ordering[A] {
      def compare(x: A, y: A) = ord.compare(y, x)
    })
    for (i <- 1 until resarr.length) revq += resarr(i)
    revq
  }

  override def reverseIterator = new Iterator[A] {
    val arr = new Array[Any](size)
    iterator.copyToArray(arr)
    var i = arr.size - 1
    def hasNext: Boolean = i >= 0
    def next(): A = {
      val curr = arr(i)
      i -= 1
      curr.asInstanceOf[A]
    }
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

  // def printstate {
  //   println("-----------------------")
  //   println("Size: " + resarr.p_size0)
  //   println("Internal array: " + resarr.p_array.toList)
  //   println(toString)
  // }
}







