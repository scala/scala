/* TODO: Reintegrate
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


//import Predef.NoSuchElementException

object Queue {
  val Empty: Queue[Nothing] = new Queue()
}

/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Erik Stenman
 *  @version 1.0, 08/07/2003
 */
@serializable
class Queue[+A](elem: A*) extends Seq[A] {

  protected val in: List[A] = Nil
  protected val out: List[A] = elem.iterator.toList

  protected def mkQueue[A](i: List[A], o: List[A]): Queue[A] =
    new Queue[A]() {
      override protected val in = i
      override protected val out = o
    }

  /** Returns the <code>n</code>-th element of this queue.
   *  The first element is at position 0.
   *
   *  @param  n index of the element to return
   *  @return   the element at position <code>n</code> in this queue.
   *  @throws Predef.NoSuchElementException if the queue is too short.
   */
  def apply(n: Int): A = {
    val len = out.length
    if (n < len) out.apply(n)
    else {
      val m = n - len
      if (m < in.length) in.reverse.apply(m)
      else throw new NoSuchElementException("index out of range")
    }
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
   *  @deprecated Use the method <code>enqueue</code> from now on.
   *
   *  @param  elem        the element to insert
   */
  @deprecated def +[B >: A](elem: B) = mkQueue(elem :: in, out)

  /** Creates a new queue with element added at the end
   *  of the old queue.
   *
   *  @param  elem        the element to insert
   */
  def enqueue[B >: A](elem: B) = mkQueue(elem :: in, out)

  /** Returns a new queue with all all elements provided by
   *  an <code>Iterable</code> object added at the end of
   *  the queue.
   *  The elements are prepended in the order they
   *  are given out by the iterator.
   *
   *  @deprecated Use the method <code>enqueue</code> from now on.
   *
   *  @param  iter        an iterable object
   */
  @deprecated def +[B >: A](iter: Iterable[B]) = {
    var q: List[B] = in
    iter.iterator.foreach(e => q = e :: q)
    mkQueue(q, out)
  }

  /** Returns a new queue with all all elements provided by
   *  an <code>Iterable</code> object added at the end of
   *  the queue.
   *  The elements are prepended in the order they
   *  are given out by the iterator.
   *
   *  @param  iter        an iterable object
   */
  def enqueue[B >: A](iter: Iterable[B]) = {
    var q: List[B] = in
    iter.iterator.foreach(e => q = e :: q)
    mkQueue(q, out)
  }

  /** Returns a tuple with the first element in the queue,
   *  and a new queue with this element removed.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element of the queue.
   */
  def dequeue: (A, Queue[A]) = {
    val (newOut, newIn) =
      if (out.isEmpty) (in.reverse, Nil)
      else (out, in)
    if (newOut.isEmpty) throw new NoSuchElementException("queue empty")
    else (newOut.head, mkQueue(newIn, newOut.tail))
  }

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element.
   */
  def front: A =
    if (out.isEmpty) {
      if (in.isEmpty) throw new NoSuchElementException("queue empty") else in.last
    } else
      out.head

  /** Returns a string representation of this queue.
   */
  override def toString() = mkString("Queue(", ", ", ")")

  /** Compares two queues for equality by comparing
   *  each element in the queues.
   *
   *  @return true, iff the two queues are structurally equal.
   */
  override def equals(o: Any): Boolean = o match {
    case q: Queue[_] =>
      /* A function that compares the element at
         position index in q with the element at
         the same position in this (queue).
         If they are equal the next element is
         compared. */
      def eqe(index: Int): Boolean = (
        /* If all elements are compared
        the queues are equal. */
        index >= this.length ||
        /* Otherwise: compare the elements */
        (q.apply(index) == this.apply(index) &&
         /* if they are equal compare the rest. */
         eqe(index + 1))
      );
        /* If the length of the ques are the same,
         compare each element, starting at index 0. */
      (q.length == this.length) && eqe(0);

    case _ => false /* o is not a queue: not equal to this. */
  }

  override def hashCode(): Int =
    if (isEmpty) 0
    else {
      val q: (A,Queue[A]) = dequeue;
      q._1.hashCode() + q._2.hashCode()
    }
}
*/
