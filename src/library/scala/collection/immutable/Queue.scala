/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection
package immutable

import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{Builder, ListBuffer}

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
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#immutable-queues "Scala's Collection Library overview"]]
  *  section on `Immutable Queues` for more information.
  *
  *  @define Coll `immutable.Queue`
  *  @define coll immutable queue
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */

sealed class Queue[+A] protected(protected val in: List[Any /* A | Many[A] */], protected val out: List[A])
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedLinearSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedSeqOps[A, Queue, Queue[A]]
    with IterableFactoryDefaults[A, Queue]
    with DefaultSerializable {

  override def iterableFactory: SeqFactory[Queue] = Queue

  /** Returns the `n`-th element of this queue.
    *  The first element is at position `0`.
    *
    *  @param  n index of the element to return
    *  @return   the element at position `n` in this queue.
    *  @throws java.util.NoSuchElementException if the queue is too short.
    */
  override def apply(n: Int): A = {
    def indexOutOfRange(): Nothing = throw new IndexOutOfBoundsException(n.toString)

    var index = 0
    var curr = out

    while (index < n && curr.nonEmpty) {
      index += 1
      curr = curr.tail
    }

    if (index == n) {
      if (curr.nonEmpty) curr.head
      else if (in.nonEmpty) in.last match {
        case Queue.Many(s) => s.head.asInstanceOf[A]
        case a => a.asInstanceOf[A]
      }
      else indexOutOfRange()
    } else {
      // TODO: this should be more optimized
      iterator.drop(n).next()
    }
  }

  override def iterator: Iterator[A] =
    new AbstractIterator[A] {
      // TODO: build outside of the class
      private[this] var currentOut: List[A] = out
      private[this] var currentInIterator: Iterator[Any] = _
      private[this] var currentManyIterator: Iterator[Any] = _

      private[this] def getCurrentInIterator(): Iterator[Any] = {
        if (currentInIterator == null) {
          currentInIterator = in.reverseIterator
        }
        currentInIterator
      }

      override def hasNext: Boolean =
        currentOut.nonEmpty ||
          (currentManyIterator != null && currentManyIterator.hasNext) ||
          getCurrentInIterator().hasNext

      override def next(): A = {
        if (currentOut.nonEmpty) {
          val result = currentOut.head
          currentOut = currentOut.tail
          result
        } else if(currentManyIterator != null && currentManyIterator.hasNext) {
            currentManyIterator.next().asInstanceOf[A]
        } else {
          getCurrentInIterator().next() match {
            case Queue.Many(s) =>
              currentManyIterator = s.iterator
              currentManyIterator.next().asInstanceOf[A]
            case a =>
              a.asInstanceOf[A]
          }
        }
      }
    }

  /** Checks if the queue is empty.
    *
    *  @return true, iff there is no element in the queue.
    */
  override def isEmpty: Boolean = in.isEmpty && out.isEmpty

  override def head: A =
    if (out.nonEmpty) out.head
    else if (in.nonEmpty) in.last match {
      case Queue.Many(s) => s.head.asInstanceOf[A]
      case a => a.asInstanceOf[A]
    }
    else throw new NoSuchElementException("head on empty queue")

  private[this] def inToOut(): List[A] = {
    var result: List[Any] = Nil
    var curr: List[Any] = in
    while (curr.nonEmpty) {
      result = curr.head match {
        case Queue.Many(s) => result.prependedAll(s)
        case a => a :: result
      }
      curr = curr.tail
    }
    result.asInstanceOf[List[A]]
  }

  override def tail: Queue[A] =
    if (out.nonEmpty) new Queue(in, out.tail)
    else if (in.nonEmpty) new Queue(Nil, inToOut().tail)
    else throw new NoSuchElementException("tail on empty queue")

  /* This is made to avoid inefficient implementation of iterator. */
  override def forall(p: A => Boolean): Boolean =
    out.forall(p) && in.forall{
      case Queue.Many(s) => s.asInstanceOf[Seq[A]].forall(p)
      case a => p(a.asInstanceOf[A])
    }

  /* This is made to avoid inefficient implementation of iterator. */
  override def exists(p: A => Boolean): Boolean =
    out.exists(p) || in.exists {
      case Queue.Many(s) => s.asInstanceOf[Seq[A]].exists(p)
      case a => p(a.asInstanceOf[A])
    }

  override protected[this] def className = "Queue"

  /** Returns the length of the queue. */
  override def length: Int = out.length + in.foldLeft(0){
    case (i, Queue.Many(s)) => i + s.length
    case (i, _) => i + 1
  }

  override def prepended[B >: A](elem: B): Queue[B] = new Queue(in, elem :: out)

  override def appended[B >: A](elem: B): Queue[B] = enqueue(elem)

  override def appendedAll[B >: A](that: scala.collection.IterableOnce[B]): Queue[B] = {
    val newIn = that match {
      case that: Queue[B] => that.in ++ (that.out reverse_::: this.in)
      case h :: Nil => h :: this.in
      case that: Seq[A] if that.isInstanceOf[List[_]] || that.isInstanceOf[Vector[_]] =>
        if (that.isEmpty) this.in
        else Queue.Many(that) :: this.in
      case _ =>
        var result: List[Any] = this.in
        val iter = that.iterator
        while (iter.hasNext) {
          result = iter.next() :: result
        }
        result
    }
    if (newIn eq this.in) this else new Queue[B](newIn, this.out)
  }

  /** Creates a new queue with element added at the end
    *  of the old queue.
    *
    *  @param  elem        the element to insert
    */
  def enqueue[B >: A](elem: B): Queue[B] = new Queue(elem :: in, out)

  /** Creates a new queue with all elements provided by an `Iterable` object
    *  added at the end of the old queue.
    *
    *  The elements are appended in the order they are given out by the
    *  iterator.
    *
    *  @param  iter        an iterable object
    */
  @deprecated("Use `enqueueAll` instead of `enqueue` to enqueue a collection of elements", "2.13.0")
  @`inline` final def enqueue[B >: A](iter: scala.collection.Iterable[B]) = enqueueAll(iter)

  /** Creates a new queue with all elements provided by an `Iterable` object
    *  added at the end of the old queue.
    *
    *  The elements are appended in the order they are given out by the
    *  iterator.
    *
    *  @param  iter        an iterable object
    */
  def enqueueAll[B >: A](iter: scala.collection.Iterable[B]): Queue[B] = appendedAll(iter)

  /** Returns a tuple with the first element in the queue,
    *  and a new queue with this element removed.
    *
    *  @throws java.util.NoSuchElementException
    *  @return the first element of the queue.
    */
  def dequeue: (A, Queue[A]) = out match {
    case Nil if !in.isEmpty => val newOut = inToOut() ; (newOut.head, new Queue(Nil, newOut.tail))
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
  override def toString(): String = mkString("Queue(", ", ", ")")
}

/** $factoryInfo
  *  @define Coll `immutable.Queue`
  *  @define coll immutable queue
  */
@SerialVersionUID(3L)
object Queue extends StrictOptimizedSeqFactory[Queue] {
  def newBuilder[A]: Builder[A, Queue[A]] = new ListBuffer[A] mapResult (x => new Queue[A](Nil, x))

  def from[A](source: IterableOnce[A]): Queue[A] = source match {
    case q: Queue[A] => q
    case _ =>
      val list = List.from(source)
      if (list.isEmpty) empty
      else new Queue(Nil, list)
  }

  def empty[A]: Queue[A] = EmptyQueue
  override def apply[A](xs: A*): Queue[A] = new Queue[A](Nil, xs.toList)

  private object EmptyQueue extends Queue[Nothing](Nil, Nil) { }

  private final case class Many[+A](seq: Seq[A])
}

