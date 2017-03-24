package strawman.collection.mutable

import scala._

/** A stack implements a data structure which allows to store and retrieve
  *  objects in a last-in-first-out (LIFO) fashion.
  *
  *  @tparam A    type of the elements contained in this stack.
  *
  *  @author  Pathikrit Bhowmick
  *  @version 2.13
  *  @since   2.13
  */
class Stack[A] extends ArrayDeque[A] {

  /**
    * Add elements to the top of this stack
    *
    * @param elem
    * @return
    */
  def push(elem: A): this.type = elem +=: this

  /** Push two or more elements onto the stack. The last element
    *  of the sequence will be on top of the new stack.
    *
    *  @param   elems      the element sequence.
    *  @return the stack with the new elements on top.
    */
  def push(elem1: A, elem2: A, elems: A*): this.type = push(elem1).push(elem2).pushAll(elems)

  /** Push all elements in the given traversable object onto the stack. The
    *  last element in the traversable object will be on top of the new stack.
    *
    *  @param elems the traversable object.
    *  @return the stack with the new elements on top.
    */
  def pushAll(elems: strawman.collection.IterableOnce[A]): this.type = elems ++=: this

  /**
    * Removes the top element from this stack and return it
    *
    * @return
    * @throws java.util.NoSuchElementException when stack is empty
    */
  def pop(): A = removeHead()

  /**
    * Pop all elements from this stack and return it
    *
    * @return
    */
  def popAll(): strawman.collection.Seq[A] = removeAll()

  /**
    * Returns and removes all elements from the top of this stack which satisfy the given predicate
    *
    *  @param f   the predicate used for choosing elements
    *  @return
    */
  def popWhile(f: A => Boolean): strawman.collection.Seq[A] = removeHeadWhile(f)
}
