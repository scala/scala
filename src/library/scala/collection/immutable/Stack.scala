/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.{ ArrayBuffer, Builder }

object Stack extends SeqFactory[Stack] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Stack[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Stack[A]] = new ArrayBuffer[A] mapResult (buf => new Stack(buf.toList))

  @deprecated("Use Stack.empty instead")
  val Empty: Stack[Nothing] = Stack()
}

/** This class implements immutable stacks using a list-based data
 *  structure.
 *
 *  @note    This class exists only for historical reason and as an
 *           analogue of mutable stacks
 *           Instead of an immutable stack you can just use a list.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/07/2003
 *  @since   1
 */
@serializable @SerialVersionUID(1976480595012942526L)
class Stack[+A] protected (protected val elems: List[A]) extends LinearSeq[A]
                    with GenericTraversableTemplate[A, Stack]
                    with LinearSeqLike[A, Stack[A]] {
  override def companion: GenericCompanion[Stack] = Stack

  def this() = this(Nil)

  /** Checks if this stack is empty.
   *
   *  @return true, iff there is no element on the stack.
   */
  override def isEmpty: Boolean = elems.isEmpty

  override def head = elems.head
  override def tail = new Stack(elems.tail)

  /** Push an element on the stack.
   *
   *  @param   elem       the element to push on the stack.
   *  @return the stack with the new element on top.
   */
  def push[B >: A](elem: B): Stack[B] = new Stack(elem :: elems)

  /** Push a sequence of elements onto the stack. The last element
   *  of the sequence will be on top of the new stack.
   *
   *  @param   elems      the element sequence.
   *  @return the stack with the new elements on top.
   */
  def push[B >: A](elem1: B, elem2: B, elems: B*): Stack[B] =
    this.push(elem1).push(elem2).pushAll(elems)

  /** Push all elements provided by the given iterator object onto
   *  the stack. The last element returned by the iterable object
   *  will be on top of the new stack.
   *
   *  @param   elems      the iterator object.
   *  @return the stack with the new elements on top.
   */
  def pushAll[B >: A](elems: Iterator[B]): Stack[B] =
    ((this: Stack[B]) /: elems)(_ push _)

  /** Push all elements provided by the given traversable object onto
   *  the stack. The last element returned by the iterable object
   *  will be on top of the new stack.
   *
   *  @param   elems      the iterable object.
   *  @return the stack with the new elements on top.
   */
  def pushAll[B >: A](elems: scala.collection.Traversable[B]): Stack[B] =
    ((this: Stack[B]) /: elems)(_ push _)

  /** Returns the top element of the stack. An error is signaled if
   *  there is no element on the stack.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the top element.
   */
  def top: A =
    if (!isEmpty) elems.head
    else throw new NoSuchElementException("top of empty stack")

  /** Removes the top element from the stack.
   *  Note: should return <code>(A, Stack[A])</code> as for queues (mics)
   *
   *  @throws Predef.NoSuchElementException
   *  @return the new stack without the former top element.
   */
  def pop: Stack[A] =
    if (!isEmpty) new Stack(elems.tail)
    else throw new NoSuchElementException("pop of empty stack")

  def pop2: (A, Stack[A]) =
    if (!isEmpty) (elems.head, new Stack(elems.tail))
    else throw new NoSuchElementException("pop of empty stack")

  override def reverse: Stack[A] = new Stack(elems.reverse)

  /** Returns an iterator over all elements on the stack. The iterator
   *  issues elements in the reversed order they were inserted into the
   *  stack (LIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  override def iterator: Iterator[A] = elems.iterator

  /** Returns a string representation of this stack.
   */
  override def toString() = elems.mkString("Stack(", ", ", ")")
}

