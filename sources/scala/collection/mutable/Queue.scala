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
 *  @version 1.0, 08/07/2003
 */
class Queue[A] with MutableList[A] {

    /** Checks if the queue is empty.
     *
     *  @returns true, iff there is no element in the queue.
     */
    def isEmpty: Boolean = (first == null);

    /** Inserts a single element at the end of the queue.
     *
     *  @param  elem        the element to insert
     */
    def +=(elem: A) = appendElem(elem);

    /** Adds all elements provided by an <code>Iterable</code> object
     *  at the end of the queue. The elements are prepended in the order they
     *  are given out by the iterator.
     *
     *  @param  iter        an iterable object
     */
    def +=(iter: Iterable[A]) = iter.elements.foreach(e => appendElem(e));

    /** Adds all elements to the queue.
     *
     *  @param  elems		the elements to add.
     */
    def enqueue(elems: A*): Unit = (this += elems);

    /** Returns the first element in the queue, and removes this element
     *  from the queue.
     *
     *  @returns the first element of the queue.
     */
    def dequeue: A = {
        if (first == null)
            error("queue empty");
        else {
            val res = first.elem;
            first = first.next;
            res;
        }
    }

    /** Returns the first element in the queue, or throws an error if there
     *  is no element contained in the queue.
     *
     *  @returns the first element.
     */
    def front: A = first.elem;

    /** Removes all elements from the queue. After this operation is completed,
     *  the queue will be empty.
     */
    def clear: Unit = reset;
}
