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

/** This class servers as a proxy for priority queues. The
 *  elements of the queue have to be ordered in terms of the
 *  `Ordered[T]` class.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 */
@deprecated("Proxying is deprecated due to lack of use and compiler-level support.", "2.11.0")
abstract class PriorityQueueProxy[A](implicit ord: Ordering[A]) extends PriorityQueue[A]
         with Proxy
{
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
