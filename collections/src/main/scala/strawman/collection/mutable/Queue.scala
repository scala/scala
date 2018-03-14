/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman.collection
package mutable

import scala.{Array, AnyRef, Boolean, Int, Option, None, Some, SerialVersionUID, Serializable, `inline`, deprecated}

import strawman.collection.toNewSeq

/** `Queue` objects implement data structures that allow to
  *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
  *
  *  @author  Pathikrit Bhowmick
  *
  *  @version 2.13
  *  @since   2.13
  *
  *  @define Coll `mutable.Queue`
  *  @define coll mutable queue
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@SerialVersionUID(3L)
class Queue[A] protected (array: Array[AnyRef], start: Int, end: Int)
  extends ArrayDeque[A](array, start, end)
    with IndexedSeqOps[A, Queue, Queue[A]]
    with StrictOptimizedSeqOps[A, Queue, Queue[A]]
    with Cloneable[Queue[A]]
    with Serializable {

  def this(initialSize: Int = ArrayDeque.DefaultInitialSize) =
    this(ArrayDeque.alloc(initialSize), start = 0, end = 0)

  override def iterableFactory: SeqFactory[Queue] = Queue

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
  def enqueue(elem1: A, elem2: A, elems: A*): this.type = enqueue(elem1).enqueue(elem2).enqueueAll(elems.toStrawman)

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

  /** Returns the first element in the queue which satisfies the
    *  given predicate, and removes this element from the queue.
    *
    *  @param p   the predicate used for choosing the first element
    *  @return the first element of the queue for which p yields true
    */
  def dequeueFirst(p: A => Boolean): Option[A] =
    if (isEmpty) None
    else if (p(head)) {
      val res = Some(head)
      removeHead()
      res
    } else {
      val i = lastIndexWhere(p)
      if (i < 0) None
      else Some(remove(i))
    }

  /**
    * Returns and removes all elements from the end of this queue which satisfy the given predicate
    *
    *  @param f   the predicate used for choosing elements
    *  @return
    */
  def dequeueAll(f: A => Boolean): strawman.collection.immutable.Seq[A] = removeHeadWhile(f)

  /** Returns the first element in the queue, or throws an error if there
    *  is no element contained in the queue.
    *
    *  @return the first element.
    */
  @`inline` final def front: A = head

  override def clone(): Queue[A] = {
    val bf = newSpecificBuilder()
    bf ++= this
    bf.result()
  }

  override protected def ofArray(array: Array[AnyRef], end: Int): Queue[A] =
    new Queue(array, start = 0, end)

}

/**
  * $factoryInfo
  * @define coll queue
  * @define Coll `Queue`
  */
object Queue extends StrictOptimizedSeqFactory[Queue] {

  def from[A](source: IterableOnce[A]): Queue[A] = empty ++= source

  def empty[A]: Queue[A] = new Queue

  def newBuilder[A](): Builder[A, Queue[A]] = new GrowableBuilder[A, Queue[A]](empty)

}