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


/** This is a synchronized version of the `Queue[T]` class. It
 *  implements a data structure that allows one to insert and retrieve
 *  elements in a first-in-first-out (FIFO) manner.
 *
 *  @tparam A     type of elements contained in this synchronized queue.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 *  @define Coll `SynchronizedQueue`
 *  @define coll synchronized queue
 */
@deprecated("Synchronization via selective overriding of methods is inherently unreliable. Consider java.util.concurrent.ConcurrentLinkedQueue as an alternative.", "2.11.0")
class SynchronizedQueue[A] extends Queue[A] {
  /** Checks if the queue is empty.
   *
   *  @return true, iff there is no element in the queue.
   */
  override def isEmpty: Boolean = synchronized { super.isEmpty }

  /** Inserts a single element at the end of the queue.
   *
   *  @param  elem        the element to insert
   */
  override def +=(elem: A): this.type = synchronized[this.type] { super.+=(elem) }

  /** Adds all elements provided by a `TraversableOnce` object
   *  at the end of the queue. The elements are prepended in the order they
   *  are given out by the iterator.
   *
   *  @param  xs        a traversable object
   */
  override def ++=(xs: TraversableOnce[A]): this.type = synchronized[this.type] { super.++=(xs) }

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  override def enqueue(elems: A*): Unit = synchronized { super.++=(elems) }

  /** Returns the first element in the queue, and removes this element
   *  from the queue.
   *
   *  @return the first element of the queue.
   */
  override def dequeue(): A = synchronized { super.dequeue() }

  /** Returns the first element in the queue which satisfies the
   *  given predicate, and removes this element from the queue.
   *
   *  @param p   the predicate used for choosing the first element
   *  @return the first element of the queue for which p yields true
   */
  override def dequeueFirst(p: A => Boolean): Option[A] = synchronized { super.dequeueFirst(p) }

  /** Returns all elements in the queue which satisfy the
   *  given predicate, and removes those elements from the queue.
   *
   *  @param p   the predicate used for choosing elements
   *  @return    a sequence of all elements in the queue for which
   *             p yields true.
   */
  override def dequeueAll(p: A => Boolean): Seq[A] = synchronized { super.dequeueAll(p) }

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   */
  override def front: A = synchronized { super.front }

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  override def clear(): Unit = synchronized { super.clear() }

  /** Checks if two queues are structurally identical.
   *
   *  @return true, iff both queues contain the same sequence of elements.
   */
  override def equals(that: Any): Boolean = synchronized { super.equals(that) }

  /** Returns a textual representation of a queue as a string.
   *
   *  @return the string representation of this queue.
   */
  override def toString() = synchronized { super.toString() }
}
