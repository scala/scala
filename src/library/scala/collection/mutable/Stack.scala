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
import scala.collection.immutable.{List, Nil}
import scala.collection.Iterator
import scala.annotation.migration

/** Factory object for the `mutable.Stack` class.
 *
 *  $factoryInfo
 *  @define coll mutable stack
 *  @define Coll `mutable.Stack`
 */
object Stack extends SeqFactory[Stack] {
  class StackBuilder[A] extends Builder[A, Stack[A]] {
    val lbuff = new ListBuffer[A]
    def +=(elem: A) = { lbuff += elem; this }
    def clear() = lbuff.clear()
    def result = new Stack(lbuff.result)
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Stack[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, Stack[A]] = new StackBuilder[A]
  val empty: Stack[Nothing] = new Stack(Nil)
}

/** A stack implements a data structure which allows to store and retrieve
 *  objects in a last-in-first-out (LIFO) fashion.
 *
 *  @tparam A    type of the elements contained in this stack.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#stacks "Scala's Collection Library overview"]]
 *  section on `Stacks` for more information.
 *  @define Coll `Stack`
 *  @define coll stack
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@deprecated("Stack is an inelegant and potentially poorly-performing wrapper around List. Use a List assigned to a var instead.", "2.12.0")
class Stack[A] private (var elems: List[A])
extends AbstractSeq[A]
   with Seq[A]
   with SeqLike[A, Stack[A]]
   with GenericTraversableTemplate[A, Stack]
   with Cloneable[Stack[A]]
   with Serializable
{
  def this() = this(Nil)

  override def companion = Stack

  /** Checks if the stack is empty.
   *
   *  @return true, iff there is no element on the stack
   */
  override def isEmpty: Boolean = elems.isEmpty

  /** The number of elements in the stack */
  override def length = elems.length

  /** Retrieve `n`-th element from stack, where top of stack has index `0`.
   *
   *  This is a linear time operation.
   *
   *  @param index     the index of the element to return
   *  @return          the element at the specified index
   *  @throws IndexOutOfBoundsException if the index is out of bounds
   */
  override def apply(index: Int) = elems(index)

  /** Replace element at index `n` with the new element `newelem`.
   *
   *  This is a linear time operation.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   *  @throws   IndexOutOfBoundsException if the index is not valid
   */
  def update(n: Int, newelem: A) =
    if(n < 0 || n >= length) throw new IndexOutOfBoundsException(n.toString)
    else elems = elems.take(n) ++ (newelem :: elems.drop(n+1))

  /** Push an element on the stack.
   *
   *  @param   elem       the element to push on the stack.
   *  @return the stack with the new element on top.
   */
  def push(elem: A): this.type = { elems = elem :: elems; this }

  /** Push two or more elements onto the stack. The last element
   *  of the sequence will be on top of the new stack.
   *
   *  @param   elems      the element sequence.
   *  @return the stack with the new elements on top.
   */
  def push(elem1: A, elem2: A, elems: A*): this.type =
    this.push(elem1).push(elem2).pushAll(elems)

  /** Push all elements in the given traversable object onto the stack. The
   *  last element in the traversable object will be on top of the new stack.
   *
   *  @param xs the traversable object.
   *  @return the stack with the new elements on top.
   */
  def pushAll(xs: TraversableOnce[A]): this.type = { xs foreach push ; this }

  /** Returns the top element of the stack. This method will not remove
   *  the element from the stack. An error is signaled if there is no
   *  element on the stack.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the top element
   */
  def top: A =
    elems.head

  /** Removes the top element from the stack.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the top element
   */
  def pop(): A = {
    val res = elems.head
    elems = elems.tail
    res
  }

  /**
   * Removes all elements from the stack. After this operation completed,
   * the stack will be empty.
   */
  def clear(): Unit = elems = Nil

  /** Returns an iterator over all elements on the stack. This iterator
   *  is stable with respect to state changes in the stack object; i.e.
   *  such changes will not be reflected in the iterator. The iterator
   *  issues elements in the reversed order they were inserted into the
   *  stack (LIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  @migration("`iterator` traverses in FIFO order.", "2.8.0")
  override def iterator: Iterator[A] = elems.iterator

  /** Creates a list of all stack elements in LIFO order.
   *
   *  @return the created list.
   */
  @migration("`toList` traverses in FIFO order.", "2.8.0")
  override def toList: List[A] = elems

  @migration("`foreach` traverses in FIFO order.", "2.8.0")
  override def foreach[U](f: A => U): Unit = super.foreach(f)

  /** This method clones the stack.
   *
   *  @return  a stack with the same elements.
   */
  override def clone(): Stack[A] = new Stack[A](elems)
}
