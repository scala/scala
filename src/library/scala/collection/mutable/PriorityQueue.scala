/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import generic._

/** This class implements priority queues using a heap.
 *  To prioritize elements of type A there must be an implicit
 *  Ordering[A] available at creation.
 *
 *  Only the `dequeue` and `dequeueAll` methods will return elements in priority
 *  order (while removing elements from the heap).  Standard collection methods
 *  including `drop`, `iterator`, and `toString` will remove or traverse the heap
 *  in whichever order seems most convenient.
 *
 *  Therefore, printing a `PriorityQueue` will not reveal the priority order of
 *  the elements, though the highest-priority element will be printed first.  To
 *  print the elements in order, one must duplicate the `PriorityQueue` (by using
 *  `clone`, for instance) and then dequeue them:
 *
 *  @example {{{
 *  val pq = collection.mutable.PriorityQueue(1, 2, 5, 3, 7)
 *  println(pq)                  // elements probably not in order
 *  println(pq.clone.dequeueAll) // prints Vector(7, 5, 3, 2, 1)
 *  }}}
 *
 *  @tparam A    type of the elements in this priority queue.
 *  @param ord   implicit ordering used to compare the elements of type `A`.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 *
 *  @define Coll PriorityQueue
 *  @define coll priority queue
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed class PriorityQueue[A](implicit val ord: Ordering[A])
   extends AbstractIterable[A]
      with Iterable[A]
      with GenericOrderedTraversableTemplate[A, PriorityQueue]
      with IterableLike[A, PriorityQueue[A]]
      with Growable[A]
      with Builder[A, PriorityQueue[A]]
      with Serializable
      with scala.Cloneable
{
  import ord._

  private class ResizableArrayAccess[A] extends AbstractSeq[A] with ResizableArray[A] with Serializable {
    def p_size0 = size0
    def p_size0_=(s: Int) = size0 = s
    def p_array = array
    def p_ensureSize(n: Int) = super.ensureSize(n)
    def p_swap(a: Int, b: Int) = super.swap(a, b)
  }

  protected[this] override def newBuilder = PriorityQueue.newBuilder[A]

  private val resarr = new ResizableArrayAccess[A]

  resarr.p_size0 += 1                  // we do not use array(0)
  def length: Int = resarr.length - 1  // adjust length accordingly
  override def size: Int = length
  override def isEmpty: Boolean = resarr.p_size0 < 2
  override def repr = this

  def result = this

  override def orderedCompanion = PriorityQueue

  private def toA(x: AnyRef): A = x.asInstanceOf[A]
  protected def fixUp(as: Array[AnyRef], m: Int): Unit = {
    var k: Int = m
    while (k > 1 && toA(as(k / 2)) < toA(as(k))) {
      resarr.p_swap(k, k / 2)
      k = k / 2
    }
  }

  protected def fixDown(as: Array[AnyRef], m: Int, n: Int): Boolean = {
    // returns true if any swaps were done (used in heapify)
    var k: Int = m
    while (n >= 2 * k) {
      var j = 2 * k
      if (j < n && toA(as(j)) < toA(as(j + 1)))
        j += 1
      if (toA(as(k)) >= toA(as(j)))
        return k != m
      else {
        val h = as(k)
        as(k) = as(j)
        as(j) = h
        k = j
      }
    }
    k != m
  }

  /** Inserts a single element into the priority queue.
   *
   *  @param  elem        the element to insert.
   *  @return             this $coll.
   */
  def +=(elem: A): this.type = {
    resarr.p_ensureSize(resarr.p_size0 + 1)
    resarr.p_array(resarr.p_size0) = elem.asInstanceOf[AnyRef]
    fixUp(resarr.p_array, resarr.p_size0)
    resarr.p_size0 += 1
    this
  }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    val from = resarr.p_size0
    for (x <- xs) unsafeAdd(x)
    heapify(from)
    this
  }

  private def unsafeAdd(elem: A): Unit = {
    // like += but skips fixUp, which breaks the ordering invariant
    // a series of unsafeAdds MUST be followed by heapify
    resarr.p_ensureSize(resarr.p_size0 + 1)
    resarr.p_array(resarr.p_size0) = elem.asInstanceOf[AnyRef]
    resarr.p_size0 += 1
  }

  private def heapify(from: Int): Unit = {
    // elements at indices 1..from-1 were already in heap order before any adds
    // elements at indices from..n are newly added, their order must be fixed
    val n = length

    if (from <= 2) {
      // no pre-existing order to maintain, do the textbook heapify algorithm
      for (i <- n/2 to 1 by -1) fixDown(resarr.p_array, i, n)
    }
    else if (n - from < 4) {
      // for very small adds, doing the simplest fix is faster
      for (i <- from to n) fixUp(resarr.p_array, i)
    }
    else {
      var min = from/2 // tracks the minimum element in the queue
      val queue = scala.collection.mutable.Queue[Int](min)

      // do fixDown on the parents of all the new elements
      // except the parent of the first new element, which is in the queue
      // (that parent is treated specially because it might be the root)
      for (i <- n/2 until min by -1) {
        if (fixDown(resarr.p_array, i, n)) {
          // there was a swap, so also need to fixDown i's parent
          val parent = i/2
          if (parent < min) { // make sure same parent isn't added twice
            min = parent
            queue += parent
          }
        }
      }

      while (queue.nonEmpty) {
        val i = queue.dequeue()
        if (fixDown(resarr.p_array, i, n)) {
          val parent = i/2
          if (parent < min && parent > 0) {
            // the "parent > 0" is to avoid adding the parent of the root
            min = parent
            queue += parent
          }
        }
      }
    }
  }

  /** Adds all elements provided by a `TraversableOnce` object
   *  into the priority queue.
   *
   *  @param  xs    a traversable object.
   *  @return       a new priority queue containing elements of both `xs` and `this`.
   */
  def ++(xs: GenTraversableOnce[A]): PriorityQueue[A] = { this.clone() ++= xs.seq }

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*): Unit = { this ++= elems }

  /** Returns the element with the highest priority in the queue,
   *  and removes this element from the queue.
   *
   *  @throws java.util.NoSuchElementException
   *  @return   the element with the highest priority.
   */
  def dequeue(): A =
    if (resarr.p_size0 > 1) {
      resarr.p_size0 = resarr.p_size0 - 1
      val result = resarr.p_array(1)
      resarr.p_array(1) = resarr.p_array(resarr.p_size0)
      resarr.p_array(resarr.p_size0) = null // erase reference from array
      fixDown(resarr.p_array, 1, resarr.p_size0 - 1)
      toA(result)
    } else
      throw new NoSuchElementException("no element to remove from heap")

  def dequeueAll[A1 >: A, That](implicit bf: CanBuildFrom[_, A1, That]): That = {
    val b = bf.apply()
    while (nonEmpty) {
      b += dequeue()
    }
    b.result()
  }

  /** Returns the element with the highest priority in the queue,
   *  or throws an error if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  override def head: A = if (resarr.p_size0 > 1) toA(resarr.p_array(1)) else throw new NoSuchElementException("queue is empty")

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  def clear(): Unit = { resarr.p_size0 = 1 }

  /** Returns an iterator which yields all the elements.
   *
   *  Note: The order of elements returned is undefined.
   *  If you want to traverse the elements in priority queue
   *  order, use `clone().dequeueAll.iterator`.
   *
   *  @return  an iterator over all the elements.
   */
  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private var i = 1
    def hasNext: Boolean = i < resarr.p_size0
    def next(): A = {
      val n = resarr.p_array(i)
      i += 1
      toA(n)
    }
  }

  /** Returns the reverse of this priority queue. The new priority queue has
   *  the same elements as the original, but the opposite ordering.
   *
   *  For example, the element with the highest priority in `pq` has the lowest
   *  priority in `pq.reverse`, and vice versa.
   *
   *  Ties are handled arbitrarily.  Elements with equal priority may or
   *  may not be reversed with respect to each other.
   *
   *  @return   the reversed priority queue.
   */
  def reverse = {
    val revq = new PriorityQueue[A]()(ord.reverse)
    // copy the existing data into the new array backwards
    // this won't put it exactly into the correct order,
    // but will require less fixing than copying it in
    // the original order
    val n = resarr.p_size0
    revq.resarr.p_ensureSize(n)
    revq.resarr.p_size0 = n
    val from = resarr.p_array
    val to = revq.resarr.p_array
    for (i <- 1 until n) to(i) = from(n-i)
    revq.heapify(1)
    revq
  }


  /** Returns an iterator which yields all the elements in the reverse order
   *  than that returned by the method `iterator`.
   *
   *  Note: The order of elements returned is undefined.
   *
   *  @return  an iterator over all elements sorted in descending order.
   */
  def reverseIterator: Iterator[A] = new AbstractIterator[A] {
    private var i = resarr.p_size0 - 1
    def hasNext: Boolean = i >= 1
    def next(): A = {
      val n = resarr.p_array(i)
      i -= 1
      toA(n)
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
   *
   *  Note: the order of elements is undefined.
   */
  def toQueue: Queue[A] = new Queue[A] ++= this.iterator

  /** Returns a textual representation of a queue as a string.
   *
   *  @return the string representation of this queue.
   */
  override def toString() = toList.mkString("PriorityQueue(", ", ", ")")

  /** Converts this $coll to a list.
   *
   *  Note: the order of elements is undefined.
   *
   *  @return a list containing all elements of this $coll.
   */
  override def toList = this.iterator.toList

  /** This method clones the priority queue.
   *
   *  @return  a priority queue with the same elements.
   */
  override def clone(): PriorityQueue[A] = {
    val pq = new PriorityQueue[A]
    val n = resarr.p_size0
    pq.resarr.p_ensureSize(n)
    java.lang.System.arraycopy(resarr.p_array, 1, pq.resarr.p_array, 1, n-1)
    pq.resarr.p_size0 = n
    pq
  }
}


object PriorityQueue extends OrderedTraversableFactory[PriorityQueue] {
  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, PriorityQueue[A]] = {
    new Builder[A, PriorityQueue[A]] {
      val pq = new PriorityQueue[A]
      def +=(elem: A): this.type = { pq.unsafeAdd(elem); this }
      def result(): PriorityQueue[A] = { pq.heapify(1); pq }
      def clear(): Unit = pq.clear()
    }
  }

  implicit def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, PriorityQueue[A]] = new GenericCanBuildFrom[A]
}


/** This class servers as a proxy for priority queues. The
  *  elements of the queue have to be ordered in terms of the
  *  `Ordered[T]` class.
  *
  *  @author  Matthias Zenger
  *  @version 1.0, 03/05/2004
  *  @since   1
  */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
sealed abstract class PriorityQueueProxy[A](implicit ord: Ordering[A]) extends PriorityQueue[A]  with Proxy {
  def self: PriorityQueue[A]

  /** Creates a new iterator over all elements contained in this
    *  object.
    *
    *  @return the new iterator
    */
  override def iterator: Iterator[A] = self.iterator

  /** Returns the length of this priority queue.
    */
  override def length: Int = self.length

  /** Checks if the queue is empty.
    *
    *  @return true, iff there is no element in the queue.
    */
  override def isEmpty: Boolean = self.isEmpty

  /** Inserts a single element into the priority queue.
    *
    *  @param  elem        the element to insert
    */
  override def +=(elem: A): this.type = { self += elem; this }

  /** Adds all elements provided by an iterator into the priority queue.
    *
    *  @param  it        an iterator
    */
  override def ++=(it: TraversableOnce[A]): this.type = {
    self ++= it
    this
  }

  /** Adds all elements to the queue.
    *
    *  @param  elems       the elements to add.
    */
  override def enqueue(elems: A*): Unit = self ++= elems

  /** Returns the element with the highest priority in the queue,
    *  and removes this element from the queue.
    *
    *  @return   the element with the highest priority.
    */
  override def dequeue(): A = self.dequeue()

  /** Returns the element with the highest priority in the queue,
    *  or throws an error if there is no element contained in the queue.
    *
    *  @return   the element with the highest priority.
    */
  override def head: A = self.head

  /** Removes all elements from the queue. After this operation is completed,
    *  the queue will be empty.
    */
  override def clear(): Unit = self.clear()

  /** Returns a regular queue containing the same elements.
    */
  override def toQueue: Queue[A] = self.toQueue

  /** This method clones the priority queue.
    *
    *  @return  a priority queue with the same elements.
    */
  override def clone(): PriorityQueue[A] = new PriorityQueueProxy[A] {
    def self = PriorityQueueProxy.this.self.clone()
  }
}


/** This class implements synchronized priority queues using a binary heap.
  *  The elements of the queue have to be ordered in terms of the `Ordered[T]` class.
  *
  *  @tparam A    type of the elements contained in this synchronized priority queue
  *  @param ord   implicit ordering used to compared elements of type `A`
  *
  *  @author  Matthias Zenger
  *  @version 1.0, 03/05/2004
  *  @since   1
  *  @define Coll `SynchronizedPriorityQueue`
  *  @define coll synchronized priority queue
  */
@deprecated("Comprehensive synchronization via selective overriding of methods is inherently unreliable. Consider java.util.concurrent.ConcurrentSkipListSet as an alternative.", "2.11.0")
sealed class SynchronizedPriorityQueue[A](implicit ord: Ordering[A]) extends PriorityQueue[A] {

  /** Checks if the queue is empty.
    *
    *  @return true, iff there is no element in the queue.
    */
  override def isEmpty: Boolean = synchronized { super.isEmpty }

  /** Inserts a single element into the priority queue.
    *
    *  @param  elem        the element to insert
    */
  override def +=(elem: A): this.type = {
    synchronized {
      super.+=(elem)
    }
    this
  }

  /** Adds all elements of a traversable object into the priority queue.
    *
    *  @param  xs        a traversable object
    */
  override def ++=(xs: TraversableOnce[A]): this.type = {
    synchronized {
      super.++=(xs)
    }
    this
  }

  /** Adds all elements to the queue.
    *
    *  @param  elems       the elements to add.
    */
  override def enqueue(elems: A*): Unit = synchronized { super.++=(elems) }

  /** Returns the element with the highest priority in the queue,
    *  and removes this element from the queue.
    *
    *  @return   the element with the highest priority.
    */
  override def dequeue(): A = synchronized { super.dequeue() }

  /** Returns the element with the highest priority in the queue,
    *  or throws an error if there is no element contained in the queue.
    *
    *  @return   the element with the highest priority.
    */
  override def head: A = synchronized { super.head }

  /** Removes all elements from the queue. After this operation is completed,
    *  the queue will be empty.
    */
  override def clear(): Unit = synchronized { super.clear() }

  /** Returns an iterator which yield all the elements of the priority
    *  queue in descending priority order.
    *
    *  @return  an iterator over all elements sorted in descending order.
    */
  override def iterator: Iterator[A] = synchronized { super.iterator }

  /** Checks if two queues are structurally identical.
    *
    *  @return true, iff both queues contain the same sequence of elements.
    */
  override def equals(that: Any): Boolean = synchronized { super.equals(that) }

  /** Returns a textual representation of a queue as a string.
    *
    *  @return the string representation of this queue.
    */
  override def toString(): String = synchronized { super.toString() }
}
