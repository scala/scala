/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._

/** `Queue` objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#queues "Scala's Collection Library overview"]]
 *  section on `Queues` for more information.
 *
 *  @define Coll `mutable.Queue`
 *  @define coll mutable queue
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
class Queue[A]
extends MutableList[A]
   with LinearSeqOptimized[A, Queue[A]]
   with GenericTraversableTemplate[A, Queue]
   with Cloneable[Queue[A]]
   with Serializable
{
  override def companion: GenericCompanion[Queue] = Queue

  override protected[this] def newBuilder = companion.newBuilder[A]

  private[mutable] def this(fst: LinkedList[A], lst: LinkedList[A], lng: Int) {
    this()
    first0 = fst
    last0 = lst
    len = lng
  }

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*): Unit = this ++= elems

  /** Returns the first element in the queue, and removes this element
   *  from the queue.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the first element of the queue.
   */
  def dequeue(): A =
    if (isEmpty)
      throw new NoSuchElementException("queue empty")
    else {
      val res = first0.elem
      first0 = first0.next
      decrementLength()
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
      decrementLength()
      res
    } else {
      val optElem = removeFromList(p)
      if (optElem != None) decrementLength()
      optElem
    }

  private def removeFromList(p: A => Boolean): Option[A] = {
    var leftlst = first0
    var res: Option[A] = None
    while (leftlst.next.nonEmpty && !p(leftlst.next.elem)) {
      leftlst = leftlst.next
    }
    if (leftlst.next.nonEmpty) {
      res = Some(leftlst.next.elem)
      if (leftlst.next eq last0) last0 = leftlst
      leftlst.next = leftlst.next.next
    }
    res
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
        decrementLength()
      }
      if (first0.isEmpty) res
      else removeAllFromList(p, res)
    }
  }

  private def removeAllFromList(p: A => Boolean, res: ArrayBuffer[A]): ArrayBuffer[A] = {
    var leftlst = first0
    while (leftlst.next.nonEmpty) {
      if (p(leftlst.next.elem)) {
        res += leftlst.next.elem
        if (leftlst.next eq last0) last0 = leftlst
        leftlst.next = leftlst.next.next
        decrementLength()
      } else leftlst = leftlst.next
    }
    res
  }

  /** Return the proper suffix of this list which starts with the first element that satisfies `p`.
   *  That element is unlinked from the list. If no element satisfies `p`, return None.
   */
  @deprecated("extractFirst inappropriately exposes implementation details. Use dequeue or dequeueAll.", "2.11.0")
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
        decrementLength()
        res
      }
    }
  }

  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   */
  def front: A = head


  // TODO - Don't override this just for new to create appropriate type....
  override def tail: Queue[A] = {
    val tl = new Queue[A]
    tailImpl(tl)
    tl
  }

  override def clone(): Queue[A] = {
    val bf = newBuilder
    bf ++= seq
    bf.result()
  }

  private[this] def decrementLength() {
    len -= 1
    if (len == 0) last0 = first0
  }
}


object Queue extends SeqFactory[Queue] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Queue[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, Queue[A]] = new MutableList[A] mapResult { _.toQueue }
}
