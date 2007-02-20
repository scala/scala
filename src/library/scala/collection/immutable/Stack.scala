/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


//import Predef.NoSuchElementException

object Stack {
  val Empty = new Stack[Nothing]
}

/** This class implements immutable stacks using a list-based data
 *  structure. Instances of <code>Stack</code> represent
 *  empty stacks; they can be either created by calling the constructor
 *  directly, or by applying the function <code>Stack.Empty</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/07/2003
 */
@serializable
class Stack[+A] extends Seq[A] {

  /** Checks if this stack is empty.
   *
   *  @return true, iff there is no element on the stack.
   */
  override def isEmpty: Boolean = true

  /** Returns the size of this stack.
   *
   *  @return the stack size.
   */
  def length: Int = 0

  /** Push an element on the stack.
   *
   *  @param   elem       the element to push on the stack.
   *  @return the stack with the new element on top.
   */
  def +[B >: A](elem: B): Stack[B] = new Node(elem)

  /** Push all elements provided by the given iterable object onto
   *  the stack. The last element returned by the iterable object
   *  will be on top of the new stack.
   *
   *  @param   elems      the iterable object.
   *  @return the stack with the new elements on top.
   */
  def +[B >: A](elems: Iterable[B]): Stack[B] =
    elems.foldLeft(this: Stack[B]){ (stack, elem) => stack + elem }

  /** Push a sequence of elements onto the stack. The last element
   *  of the sequence will be on top of the new stack.
   *
   *  @param   elems      the element sequence.
   *  @return the stack with the new elements on top.
   */
  def push[B >: A](elems: B*): Stack[B] = this + elems

  /** Returns the top element of the stack. An error is signaled if
   *  there is no element on the stack.
   *
   *  @return the top element.
   */
  def top: A = throw new NoSuchElementException("no element on stack")

  /** Removes the top element from the stack.
   *
   *  @return the new stack without the former top element.
   */
  def pop: Stack[A] = throw new NoSuchElementException("no element on stack")

  /** Returns the n-th element of this stack. The top element has index
   *  0, elements below are indexed with increasing numbers.
   *
   *  @param   n      the index number.
   *  @return the n-th element on the stack.
   */
  def apply(n: Int): A = throw new NoSuchElementException("no element on stack")

  /** Returns an iterator over all elements on the stack. The iterator
   *  issues elements in the reversed order they were inserted into the
   *  stack (LIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  def elements: Iterator[A] = new Iterator[A] {
    var that: Stack[A] = Stack.this;
    def hasNext = !that.isEmpty;
    def next =
      if (!hasNext) throw new NoSuchElementException("next on empty iterator")
      else { val res = that.top; that = that.pop; res }
  }

  /** Compares this stack with the given object.
   *
   *  @return true, iff the two stacks are equal; i.e. they contain the
   *          same elements in the same order.
   */
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[Stack[A]] && sameElements(obj.asInstanceOf[Stack[A]])

  /** Returns the hash code for this stack.
   *
   *  @return the hash code of the stack.
   */
  override def hashCode(): Int = 0

  /**
   * Redefines the prefix of the string representation.
   */
  override def stringPrefix: String = "Stack"

  // Here comes true magic: covariant lists with implicit tail references
  @serializable
  protected class Node[+B >: A](elem: B) extends Stack[B] {
    override def isEmpty: Boolean = false
    override def length: Int = Stack.this.length + 1
    override def top: B = elem
    override def pop: Stack[B] = Stack.this
    override def apply(n: Int): B = if (n > 0) Stack.this(n - 1) else elem
    override def hashCode(): Int = elem.hashCode() + Stack.this.hashCode()
  }

}
