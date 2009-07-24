/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import scala.annotation.tailrec

object Queue {
  val Empty: Queue[Nothing] = new Queue(Nil, Nil)
  def apply[A](elems: A*) = new Queue(Nil, elems.toList)
}

/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Erik Stenman
 *  @version 1.0, 08/07/2003
 */
@serializable
class Queue[+A] protected(
  protected val  in: List[A],
  protected val out: List[A]) extends Sequence[A]
{
  /** Returns the <code>n</code>-th element of this queue.
   *  The first element is at position 0.
   *
   *  @param  n index of the element to return
   *  @return   the element at position <code>n</code> in this queue.
   *  @throws Predef.NoSuchElementException if the queue is too short.
   */
  def apply(n: Int): A = {
    @tailrec
    def walk(i: Int, inlist: List[A], outlist: List[A]): A =
      (i == 0, inlist.isEmpty, outlist.isEmpty) match {
        case (_, true, true)       => throw new NoSuchElementException("index out of range")
        case (true, _, false)      => outlist.head
        case (true, _, true)       => inlist.last
        case (false, _, false)     => walk(i - 1, inlist, outlist.tail)
        case (false, false, true)  => walk(i - 1, Nil, inlist.reverse.tail)
      }

    walk(n, in, out)
  }

  /** Returns the elements in the list as an iterator
   */
  override def iterator: Iterator[A] = (out ::: in.reverse).iterator

  /** Checks if the queue is empty.
   *
   *  @return true, iff there is no element in the queue.
   */
  override def isEmpty: Boolean = in.isEmpty && out.isEmpty

  /** Returns the length of the queue.
   */
  def length = in.length + out.length

  /** Creates a new queue with element added at the end
   *  of the old queue.
   *
   *  @param  elem        the element to insert
   */
  @deprecated("Use the method <code>enqueue</code> from now on.")
  def +[B >: A](elem: B) = enqueue(elem)

  /** Creates a new queue with element added at the end
   *  of the old queue.
   *
   *  @param  elem        the element to insert
   */
  def enqueue[B >: A](elem: B) = new Queue(elem :: in, out)

  /** Returns a new queue with all all elements provided by
   *  an <code>Iterable</code> object added at the end of
   *  the queue.
   *  The elements are prepended in the order they
   *  are given out by the iterator.
   *
   *  @param  iter        an iterable object
   */
  @deprecated("Use the method <code>enqueue</code> from now on.")
  def +[B >: A](iter: Iterable[B]) = enqueue(iter)

  /** Returns a new queue with all elements provided by
   *  an <code>Iterable</code> object added at the end of
   *  the queue.
   *  The elements are prepended in the order they
   *  are given out by the iterator.
   *
   *  @param  iter        an iterable object
   */
  def enqueue[B >: A](iter: Iterable[B]) =
    new Queue(iter.iterator.toList.reverse ::: in, out)

  /** Returns a tuple with the first element in the queue,
   *  and a new queue with this element removed.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element of the queue.
   */
  def dequeue: (A, Queue[A]) = out match {
    case Nil if !in.isEmpty => val rev = in.reverse ; (rev.head, new Queue(Nil, rev.tail))
    case x :: xs            => (x, new Queue(in, xs))
    case _                  => throw new NoSuchElementException("queue empty")
  }

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element.
   */
  def front: A =
    if (!out.isEmpty) out.head
    else if (!in.isEmpty) in.last
    else throw new NoSuchElementException("queue empty")

  /** Returns a string representation of this queue.
   */
  override def toString() = mkString("Queue(", ", ", ")")

  /** Compares two queues for equality by comparing
   *  each element in the queues.
   *
   *  @return true, iff the two queues are structurally equal.
   */
  override def equals(o: Any): Boolean = o match {
    case q: Queue[_]  => this sameElements q
    case _            => false
  }

  override lazy val hashCode: Int =
    if (isEmpty) 0
    else dequeue match { case (x,y) => x.hashCode + y.hashCode }
}
