/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/** This class implements synchronized priority queues using a heap.
 *  The elements of the queue have to be ordered in terms of the
 *  <code>Ordered[T]</code> class.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 */
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

  /** Adds all elements provided by an <code>Iterable</code> object
   *  into the priority queue.
   *
   *  @param  iter        an iterable object
   */
  def ++=(iter: scala.collection.Iterable[A]): this.type = {
    synchronized {
      super.++=(iter)
    }
    this
  }

  /** Adds all elements provided by an iterator into the priority queue.
   *
   *  @param  it        an iterator
   */
  override def ++=(it: Iterator[A]): this.type = {
    synchronized {
      super.++=(it)
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
  override def dequeue(): A = synchronized { super.dequeue }

  /** Returns the element with the highest priority in the queue,
   *  or throws an error if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  override def max: A = synchronized { super.max }

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  override def clear(): Unit = synchronized { super.clear }

  /** Returns an iterator which yiels all the elements of the priority
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
