/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection
package mutable

import scala.{Int, Boolean, Unit, Option, Some, None, NoSuchElementException, Serializable, SerialVersionUID, deprecated}

/** `Queue` objects implement data structures that allow to
  *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
  *
  *  @author  Pathikrit Bhowmick
  *
  *  @version 2.13
  *  @since   2.13
  */
class Queue[A] extends ArrayDeque[A] {

  /**
    * Add elements to the end of this queue
    *
    * @param elem
    * @return this
    */
  def enqueue(elem: A): this.type = this += elem

  /** Enqueue two or more elements at the end of the queue. The last element
    *  of the sequence will be on end of the queue.
    *
    *  @param   elems      the element sequence.
    *  @return this
    */
  def enqueue(elem1: A, elem2: A, elems: A*): this.type = enqueue(elem1).enqueue(elem2).enqueueAll(elems)

  /** Enqueues all elements in the given traversable object into the queue. The
    *  last element in the traversable object will be on front of the new queue.
    *
    *  @param elems the traversable object.
    *  @return this
    */
  def enqueueAll(elems: strawman.collection.IterableOnce[A]): this.type = this ++= elems

  /**
    * Removes the from element from this queue and return it
    *
    * @return
    * @throws java.util.NoSuchElementException when queue is empty
    */
  def dequeue(): A = removeHead()

  /**
    * Dequeues all elements from this stack and return it
    *
    * @return
    */
  def dequeueAll(): strawman.collection.Seq[A] = removeAll()

  /**
    * Returns and removes all elements from the end of this queue which satisfy the given predicate
    *
    *  @param f   the predicate used for choosing elements
    *  @return
    */
  def dequeueWhile(f: A => Boolean): strawman.collection.Seq[A] = removeHeadWhile(f)
}
