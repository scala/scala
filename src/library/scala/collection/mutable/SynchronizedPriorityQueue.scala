/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

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
@deprecated("Comprehensive synchronization via selective overriding of methods is inherently unreliable.  Consider java.util.concurrent.ConcurrentSkipListSet as an alternative.", "2.11.0")
class SynchronizedPriorityQueue[A](implicit ord: Ordering[A]) extends PriorityQueue[A] {

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
