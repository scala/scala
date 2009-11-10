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

/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @cloneable
class Queue[A] extends MutableList[A] with Cloneable[Queue[A]] {

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*): Unit = this ++= elems

  /** Returns the first element in the queue, and removes this element
   *  from the queue.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element of the queue.
   */
  def dequeue(): A =
    if (isEmpty)
      throw new NoSuchElementException("queue empty")
    else {
      val res = first0.elem
      first0 = first0.next
      len -= 1
      res
    }

  /** Returns the first element in the queue which satisfies the
   *  given predicate, and removes this element from the queue.
   *
   *  @param p   the predicate used for choosing the first element
   *  @return the first element of the queue for which p yields true
   */
  def dequeueFirst(p: A => Boolean): Option[A] =
    if (isEmpty)
      None
    else if (p(first0.elem)) {
      val res: Option[A] = Some(first0.elem)
      first0 = first0.next
      len -= 1
      res
    } else
      extractFirst(first0, p) match {
        case None => None
        case Some(cell) => Some(cell.elem)
      }

  /** Returns all elements in the queue which satisfy the
   *  given predicate, and removes those elements from the queue.
   *
   *  @param p   the predicate used for choosing elements
   *  @return    a sequence of all elements in the queue for which
   *             p yields true.
   */
  def dequeueAll(p: A => Boolean): Seq[A] = {
    if (first0.isEmpty)
      Seq.empty
    else {
      val res = new ArrayBuffer[A]
      while ((first0.nonEmpty) && p(first0.elem)) {
        res += first0.elem
        first0 = first0.next
        len -= 1
      }
      var cell: Option[LinkedList[A]] = extractFirst(first0, p)
      while (!cell.isEmpty) {
        res += cell.get.elem
        cell = extractFirst(cell.get, p)
      }
      res
    }
  }

  /** Return the proper suffix of this list which starts with the first element that satisfies `p`.
   *  That element is unlinked from the list. If no element satisfies `p`, return None.
   */
  def extractFirst(start: LinkedList[A], p: A => Boolean): Option[LinkedList[A]] = {
    if (isEmpty) None
    else {
      var cell = start
      while ((cell.next.nonEmpty) && !p(cell.next.elem)) {
        cell = cell.next
      }
      if (cell.next.isEmpty)
        None
      else {
        val res: Option[LinkedList[A]] = Some(cell.next)
        cell.next = cell.next.next
        len -= 1
        res
      }
    }
  }

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   */
  def front: A = first0.elem
}
