/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import scala.math.Ordering

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
@SerialVersionUID(3L)
sealed class PriorityQueue[A](implicit val ord: Ordering[A])
  extends AbstractIterable[A]
    with Iterable[A]
    with IterableOps[A, Iterable, PriorityQueue[A]]
    with StrictOptimizedIterableOps[A, Iterable, PriorityQueue[A]]
    with Builder[A, PriorityQueue[A]]
    with Cloneable[PriorityQueue[A]]
    with Serializable
    with Growable[A]
{
  import ord._

  @SerialVersionUID(3L)
  private class ResizableArrayAccess[A] extends ArrayBuffer[A] {
    def p_size0 = size0
    def p_size0_=(s: Int) = size0 = s
    def p_array = array
    def p_ensureSize(n: Int) = super.ensureSize(n)
    def p_swap(a: Int, b: Int): Unit = {
      val h = array(a)
      array(a) = array(b)
      array(b) = h
    }
  }

  private val resarr = new ResizableArrayAccess[A]

  resarr.p_size0 += 1                  // we do not use array(0)
  def length: Int = resarr.length - 1  // adjust length accordingly
  override def size: Int = length
  override def isEmpty: Boolean = resarr.p_size0 < 2

  override protected[this] def fromSpecificIterable(coll: scala.collection.Iterable[A]): PriorityQueue[A] = PriorityQueue.from(coll)
  override protected[this] def newSpecificBuilder(): Builder[A, PriorityQueue[A]] = PriorityQueue.newBuilder()

  def mapInPlace(f: A => A): this.type = {
    resarr.mapInPlace(f)
    heapify(1)
    this
  }

  def result() = this

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
  def addOne(elem: A): this.type = {
    resarr.p_ensureSize(resarr.p_size0 + 1)
    resarr.p_array(resarr.p_size0) = elem.asInstanceOf[AnyRef]
    fixUp(resarr.p_array, resarr.p_size0)
    resarr.p_size0 += 1
    this
  }

  override def addAll(xs: IterableOnce[A]): this.type = {
    val from = resarr.p_size0
    for (x <- xs.iterator()) unsafeAdd(x)
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
  def ++(xs: IterableOnce[A]): PriorityQueue[A] = { this.clone() ++= xs }

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

  def dequeueAll[A1 >: A]: Seq[A1] = {
    val b = new ArrayBuffer[A1](size)
    while (nonEmpty) {
      b += dequeue()
    }
    b
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
  override def iterator(): Iterator[A] = new AbstractIterator[A] {
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

  /** Returns a regular queue containing the same elements.
    *
    *  Note: the order of elements is undefined.
    */
  def toQueue: Queue[A] = new Queue[A] ++= this.iterator()

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
  override def toList: immutable.List[A] = immutable.List.from(this.iterator())

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


object PriorityQueue extends SortedIterableFactory[PriorityQueue] {
  def newBuilder[A : Ordering](): Builder[A, PriorityQueue[A]] = {
    new Builder[A, PriorityQueue[A]] {
      val pq = new PriorityQueue[A]
      def addOne(elem: A): this.type = { pq.unsafeAdd(elem); this }
      def result(): PriorityQueue[A] = { pq.heapify(1); pq }
      def clear(): Unit = pq.clear()
    }
  }

  def empty[A : Ordering]: PriorityQueue[A] = new PriorityQueue[A]

  def from[E : Ordering](it: IterableOnce[E]): PriorityQueue[E] = {
    val b = newBuilder[E]()
    b ++= it
    b.result()
  }
}
