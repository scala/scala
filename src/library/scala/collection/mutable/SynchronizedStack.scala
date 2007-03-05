/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This is a synchronized version of the <code>Stack[T]</code> class. It
 *  implements a data structure which allows to store and retrieve
 *  objects in a last-in-first-out (LIFO) fashion.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 */
class SynchronizedStack[A] extends Stack[A] {

  /** Checks if the stack is empty.
   *
   *  @return true, iff there is no element on the stack
   */
  override def isEmpty: Boolean = synchronized { super.isEmpty }

  /** Pushes a single element on top of the stack.
   *
   *  @param  elem        the element to push onto the stack
   */
  override def +=(elem: A): Unit = synchronized { super.+=(elem) }

  /** Pushes all elements provided by an <code>Iterable</code> object
   *  on top of the stack. The elements are pushed in the order they
   *  are given out by the iterator.
   *
   *  @param  iter        an iterable object
   */
  override def ++=(iter: Iterable[A]): Unit = synchronized { super.++=(iter) }

  /** Pushes all elements provided by an iterator
   *  on top of the stack. The elements are pushed in the order they
   *  are given out by the iterator.
   *
   *  @param  iter        an iterator
   */
    override def ++=(it: Iterator[A]): Unit = synchronized { super.++=(it) }

  /** Pushes a sequence of elements on top of the stack. The first element
   *  is pushed first, etc.
   *
   *  @param  elems       a sequence of elements
   */
  override def push(elems: A*): Unit = synchronized { super.++=(elems) }

  /** Returns the top element of the stack. This method will not remove
   *  the element from the stack. An error is signaled if there is no
   *  element on the stack.
   *
   *  @return the top element
   */
  override def top: A = synchronized { super.top }

  /** Removes the top element from the stack.
   */
  override def pop(): A = synchronized { super.pop }

  /**
   * Removes all elements from the stack. After this operation completed,
   * the stack will be empty.
   */
  override def clear(): Unit = synchronized { super.clear }

  /** Returns an iterator over all elements on the stack. This iterator
   *  is stable with respect to state changes in the stack object; i.e.
   *  such changes will not be reflected in the iterator. The iterator
   *  issues elements in the order they were inserted into the stack
   *  (FIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  override def elements: Iterator[A] = synchronized { super.elements }

  /** Creates a list of all stack elements in FIFO order.
   *
   *  @return the created list.
   */
  override def toList: List[A] = synchronized { super.toList }

  /** Checks if two stacks are structurally identical.
   *
   *  @return true, iff both stacks contain the same sequence of elements.
   */
    override def equals(that: Any): Boolean = synchronized { super.equals(that) }

  /** The hashCode method always yields an error, since it is not
   *  safe to use mutable stacks as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int = synchronized { super.hashCode() }

  /** Returns a textual representation of a stack as a string.
   *
   *  @return the string representation of this stack.
   */
  override def toString() = synchronized { super.toString() }
}
