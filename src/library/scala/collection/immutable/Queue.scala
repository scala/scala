/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import generic._
import mutable.{ Builder, ListBuffer }
import scala.annotation.tailrec

/** `Queue` objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  `Queue` is implemented as a pair of `List`s, one containing the ''in'' elements and the other the ''out'' elements.
 *  Elements are added to the ''in'' list and removed from the ''out'' list. When the ''out'' list runs dry, the
 *  queue is pivoted by replacing the ''out'' list by ''in.reverse'', and ''in'' by ''Nil''.
 *
 *  Adding items to the queue always has cost `O(1)`. Removing items has cost `O(1)`, except in the case
 *  where a pivot is required, in which case, a cost of `O(n)` is incurred, where `n` is the number of elements in the queue. When this happens,
 *  `n` remove operations with `O(1)` cost are guaranteed. Removing an item is on average `O(1)`.
 *
 *  @author  Erik Stenman
 *  @version 1.0, 08/07/2003
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable_queues "Scala's Collection Library overview"]]
 *  section on `Immutable Queues` for more information.
 *
 *  @define Coll `immutable.Queue`
 *  @define coll immutable queue
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */

@SerialVersionUID(-7622936493364270175L)
@deprecatedInheritance("The implementation details of immutable queues make inheriting from them unwise.", "2.11.0")
class Queue[+A] protected(protected val in: List[A], protected val out: List[A])
         extends AbstractSeq[A]
            with LinearSeq[A]
            with GenericTraversableTemplate[A, Queue]
            with LinearSeqLike[A, Queue[A]]
            with Serializable {

  override def companion: GenericCompanion[Queue] = Queue

  /** Returns the `n`-th element of this queue.
   *  The first element is at position `0`.
   *
   *  @param  n index of the element to return
   *  @return   the element at position `n` in this queue.
   *  @throws java.util.NoSuchElementException if the queue is too short.
   */
  override def apply(n: Int): A = {
    val olen = out.length
    if (n < olen) out.apply(n)
    else {
      val m = n - olen
      val ilen = in.length
      if (m < ilen) in.apply(ilen - m - 1)
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

  override def head: A =
    if (out.nonEmpty) out.head
    else if (in.nonEmpty) in.last
    else throw new NoSuchElementException("head on empty queue")

  override def tail: Queue[A] =
    if (out.nonEmpty) new Queue(in, out.tail)
    else if (in.nonEmpty) new Queue(Nil, in.reverse.tail)
    else throw new NoSuchElementException("tail on empty queue")

  /** Returns the length of the queue.
   */
  override def length = in.length + out.length

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Queue[A], B, That]): That = bf match {
    case _: Queue.GenericCanBuildFrom[_] => new Queue(in, elem :: out).asInstanceOf[That]
    case _                               => super.+:(elem)(bf)
  }

  override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Queue[A], B, That]): That = bf match {
    case _: Queue.GenericCanBuildFrom[_] => enqueue(elem).asInstanceOf[That]
    case _                               => super.:+(elem)(bf)
  }

  /** Creates a new queue with element added at the end
   *  of the old queue.
   *
   *  @param  elem        the element to insert
   */
  def enqueue[B >: A](elem: B) = new Queue(elem :: in, out)

  /** Returns a new queue with all elements provided by an `Iterable` object
   *  added at the end of the queue.
   *
   *  The elements are appended in the order they are given out by the
   *  iterator.
   *
   *  @param  iter        an iterable object
   */
  def enqueue[B >: A](iter: Iterable[B]) =
    new Queue(iter.toList reverse_::: in, out)

  /** Returns a tuple with the first element in the queue,
   *  and a new queue with this element removed.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the first element of the queue.
   */
  def dequeue: (A, Queue[A]) = out match {
    case Nil if !in.isEmpty => val rev = in.reverse ; (rev.head, new Queue(Nil, rev.tail))
    case x :: xs            => (x, new Queue(in, xs))
    case _                  => throw new NoSuchElementException("dequeue on empty queue")
  }

  /** Optionally retrieves the first element and a queue of the remaining elements.
   *
   * @return A tuple of the first element of the queue, and a new queue with this element removed.
   *         If the queue is empty, `None` is returned.
   */
  def dequeueOption: Option[(A, Queue[A])] = if(isEmpty) None else Some(dequeue)

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the first element.
   */
  def front: A = head

  /** Returns a string representation of this queue.
   */
  override def toString() = mkString("Queue(", ", ", ")")
}

/** $factoryInfo
 *  @define Coll `immutable.Queue`
 *  @define coll immutable queue
 */
object Queue extends SeqFactory[Queue] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Queue[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Queue[A]] = new ListBuffer[A] mapResult (x => new Queue[A](Nil, x.toList))
  override def empty[A]: Queue[A] = EmptyQueue.asInstanceOf[Queue[A]]
  override def apply[A](xs: A*): Queue[A] = new Queue[A](Nil, xs.toList)

  private object EmptyQueue extends Queue[Nothing](Nil, Nil) { }
}
