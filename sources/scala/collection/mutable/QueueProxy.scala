/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 03/05/2004
 */
class QueueProxy[A](q: Queue[A]) extends Queue[A] with SeqProxy[A](q) {

    /** Access element number <code>n</code>.
     *
     *  @return  the element at index <code>n</code>.
     */
    override def apply(n: Int): A = q.apply(n);

    /** Returns the length of this queue.
     */
    override def length: Int = q.length;

    /** Checks if the queue is empty.
     *
     *  @return true, iff there is no element in the queue.
     */
    override def isEmpty: Boolean = q.isEmpty;

    /** Inserts a single element at the end of the queue.
     *
     *  @param  elem        the element to insert
     */
    override def +=(elem: A): Unit = q += elem;

    /** Adds all elements provided by an <code>Iterable</code> object
     *  at the end of the queue. The elements are prepended in the order they
     *  are given out by the iterator.
     *
     *  @param  iter        an iterable object
     */
    override def ++=(iter: Iterable[A]): Unit = q ++= iter;

    /** Adds all elements provided by an iterator
     *  at the end of the queue. The elements are prepended in the order they
     *  are given out by the iterator.
     *
     *  @param  iter        an iterator
     */
    override def ++=(it: Iterator[A]): Unit = q ++= it;

    /** Adds all elements to the queue.
     *
     *  @param  elems       the elements to add.
     */
    override def enqueue(elems: A*): Unit = q ++= elems;

    /** Returns the first element in the queue, and removes this element
     *  from the queue.
     *
     *  @return the first element of the queue.
     */
    override def dequeue: A = q.dequeue;

    /** Returns the first element in the queue, or throws an error if there
     *  is no element contained in the queue.
     *
     *  @return the first element.
     */
    override def front: A = q.front;

    /** Removes all elements from the queue. After this operation is completed,
     *  the queue will be empty.
     */
    override def clear: Unit = q.clear;

    /** Returns an iterator over all elements on the queue.
     *
     *  @return an iterator over all queue elements.
     */
    override def elements: Iterator[A] = q.elements;

    /** This method clones the queue.
     *
     *  @return  a queue with the same elements.
     */
    override def clone(): Queue[A] = new QueueProxy(q.clone());
}
