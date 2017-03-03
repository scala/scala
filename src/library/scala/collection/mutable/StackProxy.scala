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

/** A stack implements a data structure which allows to store and retrieve
 *  objects in a last-in-first-out (LIFO) fashion.
 *
 *  @tparam A   type of the elements in this stack proxy.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/05/2004
 *  @since   1
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait StackProxy[A] extends Stack[A] with Proxy {

  def self: Stack[A]

  /** Access element number `n`.
   *
   *  @return  the element at index `n`.
   */
  override def apply(n: Int): A = self.apply(n)

  /** Returns the length of this stack.
   */
  override def length: Int = self.length

  /** Checks if the stack is empty.
   *
   *  @return true, iff there is no element on the stack
   */
  override def isEmpty: Boolean = self.isEmpty

  /** Pushes a single element on top of the stack.
   *
   *  @param  elem        the element to push onto the stack
   */
  def +=(elem: A): this.type = {
    self push elem
    this
  }

  override def pushAll(xs: TraversableOnce[A]): this.type = { self pushAll xs; this }

  override def push(elem1: A, elem2: A, elems: A*): this.type = {
    self.push(elem1).push(elem2).pushAll(elems)
    this
  }

  override def push(elem: A): this.type = {
    self.push(elem)
    this
  }

  /** Returns the top element of the stack. This method will not remove
   *  the element from the stack. An error is signaled if there is no
   *  element on the stack.
   *
   *  @return the top element
   */
  override def top: A = self.top

  /** Removes the top element from the stack.
   */
  override def pop(): A = self.pop()

  /**
   * Removes all elements from the stack. After this operation completed,
   * the stack will be empty.
   */
  override def clear(): Unit = self.clear()

  /** Returns an iterator over all elements on the stack. This iterator
   *  is stable with respect to state changes in the stack object; i.e.
   *  such changes will not be reflected in the iterator. The iterator
   *  issues elements in the order they were inserted into the stack
   *  (FIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  override def iterator: Iterator[A] = self.iterator

  /** Creates a list of all stack elements in FIFO order.
   *
   *  @return the created list.
   */
  override def toList: List[A] = self.toList

  /** This method clones the stack.
   *
   *  @return  a stack with the same elements.
   */
  override def clone(): Stack[A] = new StackProxy[A] {
    def self = StackProxy.this.self.clone()
  }
}
