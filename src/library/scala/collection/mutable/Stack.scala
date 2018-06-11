package scala.collection.mutable

import scala.annotation.migration
import scala.collection.{IterableOnce, SeqFactory, StrictOptimizedSeqFactory, StrictOptimizedSeqOps}

/** A stack implements a data structure which allows to store and retrieve
  *  objects in a last-in-first-out (LIFO) fashion.
  *
  *  @tparam A    type of the elements contained in this stack.
  *
  *  @author  Pathikrit Bhowmick
  *  @since   2.13
  *
  *  @define Coll `Stack`
  *  @define coll stack
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@migration("Stack is now based on an ArrayDeque instead of a linked list", "2.13.0")
class Stack[A] protected (array: Array[AnyRef], start: Int, end: Int)
  extends ArrayDeque[A](array, start, end)
    with IndexedSeqOps[A, Stack, Stack[A]]
    with StrictOptimizedSeqOps[A, Stack, Stack[A]]
    with Cloneable[Stack[A]] {

  def this(initialSize: Int = ArrayDeque.DefaultInitialSize) =
    this(ArrayDeque.alloc(initialSize), start = 0, end = 0)

  override def iterableFactory: SeqFactory[Stack] = Stack

  override protected[this] def stringPrefix = "Stack"

  /**
    * Add elements to the top of this stack
    *
    * @param elem
    * @return
    */
  def push(elem: A): this.type = prepend(elem)

  /** Push two or more elements onto the stack. The last element
    *  of the sequence will be on top of the new stack.
    *
    *  @param   elems      the element sequence.
    *  @return the stack with the new elements on top.
    */
  def push(elem1: A, elem2: A, elems: A*): this.type = {
    val k = elems.knownSize
    ensureSize(length + (if(k >= 0) k + 2 else 3))
    prepend(elem1).prepend(elem2).pushAll(elems)
  }

  /** Push all elements in the given traversable object onto the stack. The
    *  last element in the traversable object will be on top of the new stack.
    *
    *  @param elems the traversable object.
    *  @return the stack with the new elements on top.
    */
  def pushAll(elems: scala.collection.IterableOnce[A]): this.type =
    prependAll(elems match {
      case it: scala.collection.Seq[A] => it.view.reverse
      case it => IndexedSeq.from(it).view.reverse
    })

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
    * @return The removed elements
    */
  def popAll(): scala.collection.Seq[A] = removeAllReverse()

  /**
    * Returns and removes all elements from the top of this stack which satisfy the given predicate
    *
    *  @param f   the predicate used for choosing elements
    *  @return The removed elements
    */
  def popWhile(f: A => Boolean): scala.collection.Seq[A] = removeHeadWhile(f)

  /** Returns the top element of the stack. This method will not remove
    *  the element from the stack. An error is signaled if there is no
    *  element on the stack.
    *
    *  @throws java.util.NoSuchElementException
    *  @return the top element
    */
  @`inline` final def top: A = head

  override def clone(): Stack[A] = {
    val bf = newSpecificBuilder
    bf ++= this
    bf.result()
  }

  override protected def ofArray(array: Array[AnyRef], end: Int): Stack[A] =
    new Stack(array, start = 0, end)

}

/**
  * $factoryInfo
  * @define coll stack
  * @define Coll `Stack`
  */
@SerialVersionUID(3L)
object Stack extends StrictOptimizedSeqFactory[Stack] {

  def from[A](source: IterableOnce[A]): Stack[A] = empty ++= source

  def empty[A]: Stack[A] = new Stack

  def newBuilder[A]: Builder[A, Stack[A]] = new GrowableBuilder[A, Stack[A]](empty)

}
