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
import scala.reflect.ClassTag

/** Factory object for the `ArrayStack` class.
 *
 *  $factoryInfo
 *  @define coll array stack
 *  @define Coll `ArrayStack`
 */
object ArrayStack extends SeqFactory[ArrayStack] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ArrayStack[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, ArrayStack[A]] = new ArrayStack[A]
  def empty: ArrayStack[Nothing] = new ArrayStack()
  def apply[A: ClassTag](elems: A*): ArrayStack[A] = {
    val els: Array[AnyRef] = elems.reverseMap(_.asInstanceOf[AnyRef])(breakOut)
    if (els.length == 0) new ArrayStack()
    else new ArrayStack[A](els, els.length)
  }

  private[mutable] def growArray(x: Array[AnyRef]) = {
    val y = new Array[AnyRef](math.max(x.length * 2, 1))
    Array.copy(x, 0, y, 0, x.length)
    y
  }

  private[mutable] def clone(x: Array[AnyRef]) = {
    val y = new Array[AnyRef](x.length)
    Array.copy(x, 0, y, 0, x.length)
    y
  }
}


/** Simple stack class backed by an array. Should be significantly faster
 *  than the standard mutable stack.
 *
 *  @author David MacIver
 *  @since  2.7
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#array_stacks "Scala's Collection Library overview"]]
 *  section on `Array Stacks` for more information.
 *
 *  @tparam T    type of the elements contained in this array stack.
 *
 *  @define Coll `ArrayStack`
 *  @define coll array stack
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(8565219180626620510L)
class ArrayStack[T] private(private var table : Array[AnyRef],
                            private var index : Int)
extends AbstractSeq[T]
   with Seq[T]
   with SeqLike[T, ArrayStack[T]]
   with GenericTraversableTemplate[T, ArrayStack]
   with Cloneable[ArrayStack[T]]
   with Builder[T, ArrayStack[T]]
   with Serializable
{
  def this() = this(new Array[AnyRef](1), 0)

  /** Retrieve n'th element from stack, where top of stack has index 0.
   *
   *  This is a constant time operation.
   *
   *  @param n     the index of the element to return
   *  @return      the element at the specified index
   *  @throws IndexOutOfBoundsException if the index is out of bounds
   */
  def apply(n: Int): T =
    table(index - 1 - n).asInstanceOf[T]

  /** The number of elements in the stack */
  def length = index

  override def companion = ArrayStack

  /** Replace element at index `n` with the new element `newelem`.
   *
   *  This is a constant time operation.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   *  @throws   IndexOutOfBoundsException if the index is not valid
   */
  def update(n: Int, newelem: T) =
    table(index - 1 - n) = newelem.asInstanceOf[AnyRef]

  /** Push an element onto the stack.
   *
   *  @param x The element to push
   */
  def push(x: T) {
    if (index == table.length) table = ArrayStack.growArray(table)
    table(index) = x.asInstanceOf[AnyRef]
    index += 1
  }

  /** Pop the top element off the stack.
   *
   *  @return the element on top of the stack
   */
  def pop(): T = {
    if (index == 0) sys.error("Stack empty")
    index -= 1
    val x = table(index).asInstanceOf[T]
    table(index) = null
    x
  }

  /** View the top element of the stack.
   *
   *  Does not remove the element on the top. If the stack is empty,
   *  an exception is thrown.
   *
   *  @return the element on top of the stack.
   */
  def top: T = table(index - 1).asInstanceOf[T]

  /** Duplicate the top element of the stack.
   *
   *  After calling this method, the stack will have an additional element at
   *  the top equal to the element that was previously at the top.
   *  If the stack is empty, an exception is thrown.
   */
  def dup() = push(top)

  /** Empties the stack. */
  def clear() {
    index = 0
    table = new Array(1)
  }

  /** Empties the stack, passing all elements on it in LIFO order to the
   *  provided function.
   *
   *  @param f The function to drain to.
   */
  def drain(f: T => Unit) = while (!isEmpty) f(pop())

  /** Pushes all the provided elements in the traversable object onto the stack.
   *
   *  @param xs The source of elements to push.
   *  @return   A reference to this stack.
   */
  override def ++=(xs: TraversableOnce[T]): this.type = { xs foreach += ; this }

  /** Does the same as `push`, but returns the updated stack.
   *
   *  @param x  The element to push.
   *  @return   A reference to this stack.
   */
  def +=(x: T): this.type = { push(x); this }

  def result = {
    reverseTable()
    this
  }

  private def reverseTable() {
    var i = 0
    val until = index / 2
    while (i < until) {
      val revi = index - i - 1
      val tmp = table(i)
      table(i) = table(revi)
      table(revi) = tmp
      i += 1
    }
  }

  /** Pop the top two elements off the stack, apply `f` to them and push the result
   *  back on to the stack.
   *
   *  This function will throw an exception if stack contains fewer than 2 elements.
   *
   *  @param f   The function to apply to the top two elements.
   */
  def combine(f: (T, T) => T): Unit = push(f(pop(), pop()))

  /** Repeatedly combine the top elements of the stack until the stack contains only
   *  one element.
   *
   *  @param f   The function to apply repeatedly to topmost elements.
   */
  def reduceWith(f: (T, T) => T): Unit = while(size > 1) combine(f)

  override def size = index

  /** Evaluates the expression, preserving the contents of the stack so that
   *  any changes the evaluation makes to the stack contents will be undone after
   *  it completes.
   *
   *  @param action The action to run.
   */
  def preserving[T](action: => T) = {
    val oldIndex = index
    val oldTable = ArrayStack.clone(table)

    try {
      action
    } finally {
      index = oldIndex
      table = oldTable
    }
  }

  override def isEmpty: Boolean = index == 0

  /** Creates and iterator over the stack in LIFO order.
   *  @return an iterator over the elements of the stack.
   */
  def iterator: Iterator[T] = new AbstractIterator[T] {
    var currentIndex = index
    def hasNext = currentIndex > 0
    def next() = {
      currentIndex -= 1
      table(currentIndex).asInstanceOf[T]
    }
  }

  override def foreach[U](f: T => U) {
    var currentIndex = index
    while (currentIndex > 0) {
      currentIndex -= 1
      f(table(currentIndex).asInstanceOf[T])
    }
  }

  override def clone() = new ArrayStack[T](ArrayStack.clone(table), index)
}
