/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable


/** This is a synchronized version of the `Stack[T]` class. It
 *  implements a data structure which allows to store and retrieve
 *  objects in a last-in-first-out (LIFO) fashion.
 *
 *  @tparam A    type of the elements contained in this stack.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 03/05/2004
 *  @since   1
 *  @define Coll `SynchronizedStack`
 *  @define coll synchronized stack
 */
@deprecated("Synchronization via selective overriding of methods is inherently unreliable. Consider java.util.concurrent.LinkedBlockingDequeue instead.", "2.11.0")
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
  override def push(elem: A): this.type = synchronized[this.type] { super.push(elem) }

  /** Push two or more elements onto the stack. The last element
   *  of the sequence will be on top of the new stack.
   *
   *  @param elem1      the first element to push.
   *  @param elem2      the second element to push.
   *  @param elems      the element sequence that will be pushed.
   *  @return           the stack with the new elements on top.
   */
  override def push(elem1: A, elem2: A, elems: A*): this.type = synchronized[this.type] { super.push(elem1, elem2, elems: _*) }

  /** Pushes all elements provided by a traversable object
   *  on top of the stack. The elements are pushed in the order the
   *  traversable object is traversed.
   *
   *  @param  xs        a traversable object
   */
  override def pushAll(xs: TraversableOnce[A]): this.type = synchronized[this.type] { super.pushAll(elems) }

  /** Returns the top element of the stack. This method will not remove
   *  the element from the stack. An error is signaled if there is no
   *  element on the stack.
   *
   *  @return the top element
   */
  override def top: A = synchronized { super.top }

  /** Removes the top element from the stack.
   */
  override def pop(): A = synchronized { super.pop() }

  /**
   * Removes all elements from the stack. After this operation completed,
   * the stack will be empty.
   */
  override def clear(): Unit = synchronized { super.clear() }

  /** Returns an iterator over all elements on the stack. This iterator
   *  is stable with respect to state changes in the stack object; i.e.
   *  such changes will not be reflected in the iterator. The iterator
   *  issues elements in the order they were inserted into the stack
   *  (FIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  override def iterator: Iterator[A] = synchronized { super.iterator }

  /** Creates a list of all stack elements in FIFO order.
   *
   *  @return the created list.
   */
  override def toList: List[A] = synchronized { super.toList }

  /** Returns a textual representation of a stack as a string.
   *
   *  @return the string representation of this stack.
   */
  override def toString = synchronized { super.toString }
}
