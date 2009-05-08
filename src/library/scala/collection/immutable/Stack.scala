/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
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
 *  @note    This class exists only for historical reason and as an analogue of mutable stacks
 *           Instead of an immutable stack you can just use a list.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 10/07/2003
 */
@serializable
class Stack[+A] extends Sequence[A] {

  /** Checks if this stack is empty.
   *
   *  @return true, iff there is no element on the stack.
   */
  override def isEmpty: Boolean = true

  /** The number of elements in the stack
   */
  def length: Int = 0

  /** Push an element on the stack.
   *
   *  @param   elem       the element to push on the stack.
   *  @return the stack with the new element on top.
   */
  def push[B >: A](elem: B): Stack[B] = new Node(elem)

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
   *  @deprecated
   */
  def pushAll[B >: A](elems: Iterator[B]): Stack[B] =
    ((this: Stack[B]) /: elems)(_ push _)

  /** Push all elements provided by the given traversible object onto
   *  the stack. The last element returned by the iterable object
   *  will be on top of the new stack.
   *
   *  @param   elems      the iterable object.
   *  @return the stack with the new elements on top.
   */
  def pushAll[B >: A](elems: collection.Traversible[B]): Stack[B] =
    ((this: Stack[B]) /: elems)(_ push _)

  /** Returns the top element of the stack. An error is signaled if
   *  there is no element on the stack.
   *
   *  @return the top element.
   */
  def top: A = throw new NoSuchElementException("top of empty stack")

  /** Removes the top element from the stack.
   *
   *  @return the new stack without the former top element.
   */
  def pop: Stack[A] = throw new NoSuchElementException("pop of empty stack")

  /** Returns the n-th element of this stack. The bottom element has index
   *  0, elements above are indexed with increasing numbers.
   *
   *  @param   n      the index number.
   *  @return the n-th element on the stack.
   */
  def apply(n: Int): A = if (n <= 0) top else pop.apply(n - 1)

  /** Returns an iterator over all elements on the stack. The iterator
   *  issues elements in the reversed order they were inserted into the
   *  stack (LIFO order).
   *
   *  @return an iterator over all stack elements.
   */
  override def elements: Iterator[A] = new Iterator[A] {
    private var cur = Stack.this
    def hasNext: Boolean = !cur.isEmpty
    def next: A = { val r = top; cur = cur.pop; r }
  }

  /** Returns the hash code for this stack.
   *
   *  @return the hash code of the stack.
   */
  override def hashCode(): Int = "Stack".hashCode

  /**
   * Redefines the prefix of the string representation.
   */
  override def stringPrefix: String = "Stack"

  @serializable
  protected class Node[+B >: A](elem: B) extends Stack[B] {
    override def isEmpty: Boolean = false
    override def length: Int = Stack.this.length + 1
    override def top: B = elem
    override def pop: Stack[B] = Stack.this
    override def hashCode(): Int = elem.hashCode() * 37 + Stack.this.hashCode()
  }
}

