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
import collection.immutable.{List, Nil}
import collection.Iterator

/** A stack implements a data structure which allows to store and retrieve
 *  objects in a last-in-first-out (LIFO) fashion.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @cloneable
class Stack[A] private (var elems: List[A]) extends scala.collection.Seq[A] with Cloneable[Stack[A]] {

  def this() = this(Nil)

  /** Checks if the stack is empty.
   *
   *  @return true, iff there is no element on the stack
   */
  override def isEmpty: Boolean = elems.isEmpty

  /** The number of elements in the stack */
  override def length = elems.length

  /** Retrieve n'th element from stack, where top of stack has index 0 */
  override def apply(index: Int) = elems(index)

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
  def push(elem1: A, elem2: A, elems: A*): this.type = this.push(elem1).push(elem2).pushAll(elems)

  /** Push all elements provided by the given iterator object onto
   *  the stack. The last element returned by the iterator
   *  will be on top of the new stack.
   *
   *  @param   elems      the iterator object.
   *  @return the stack with the new elements on top.
   */
  def pushAll(elems: Iterator[A]): this.type = { for (elem <- elems) { push(elem); () }; this }

  /** Push all elements provided by the given iterable object onto
   *  the stack. The last element returned by the traversable object
   *  will be on top of the new stack.
   *
   *  @param   elems      the iterable object.
   *  @return the stack with the new elements on top.
   */
  def pushAll(elems: scala.collection.Traversable[A]): this.type = { for (elem <- elems) { push(elem); () }; this }

  @deprecated("use pushAll") def ++=(it: Iterator[A]): this.type = pushAll(it)
  @deprecated("use pushAll") def ++=(it: scala.collection.Iterable[A]): this.type = pushAll(it)

  /** Returns the top element of the stack. This method will not remove
   *  the element from the stack. An error is signaled if there is no
   *  element on the stack.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the top element
   */
  def top: A =
    elems.head

  /** Removes the top element from the stack.
   *
   *  @throws Predef.NoSuchElementException
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
   *  issues elements in the reversed order they were inserted into the stack
   *  (LIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  override def iterator: Iterator[A] = elems.iterator

  /** Creates a list of all stack elements in LIFO order.
   *
   *  @return the created list.
   */
  override def toList: List[A] = elems

  /** This method clones the stack.
   *
   *  @return  a stack with the same elements.
   */
  override def clone(): Stack[A] = new Stack[A](elems)
}
