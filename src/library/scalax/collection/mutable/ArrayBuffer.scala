/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ArrayBuffer.scala 15407 2008-06-20 09:26:36Z stepancheg $


package scalax.collection.mutable

import generic.SequenceFactory

/* Factory object for `ArrayBuffer` class */
object ArrayBuffer extends SequenceFactory[ArrayBuffer] {
  def apply[A](args: A*): ArrayBuffer[A] = new ArrayBuffer[A] ++ args.asInstanceOf[Iterable[A]] // !@!
}

/** An implementation of the <code>Buffer</code> class using an array to
 *  represent the assembled sequence internally. Append, update and random
 *  access take constant time (amortized time). Prepends and removes are
 *  linear in the buffer size.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
@serializable
class ArrayBuffer[A](override protected val initialSize: Int)
  extends Buffer[A]
     with Vector[A]
     with generic.mutable.VectorTemplate[ArrayBuffer, A]
     with Builder[ArrayBuffer, A]
     with ResizableArray[A] {

  def this() = this(16)

  def clear() { reduceToSize(0) }

  /** Appends a single element to this buffer and returns
   *  the identity of the buffer. It takes constant time.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A) {
    ensureSize(size0 + 1)
    array(size0) = elem.asInstanceOf[AnyRef]
    size0 += 1
  }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterfable object.
   *  @return      the updated buffer.
   */
  override def ++=(iter: collection.Iterable[A]) = iter match {
    case v: Vector[_] =>
      val n = v.length
      ensureSize(size0 + n)
      v.copyToArray(array.asInstanceOf[scala.Array[Any]], n)
    case _ =>
      super.++=(iter)
  }

  /** Prepends a single element to this buffer and return
   *  the identity of the buffer. It takes time linear in
   *  the buffer size.
   *
   *  @param elem  the element to append.
   *  @return      the updated buffer.
   */
  def +:(elem: A): this.type = {
    ensureSize(size0 + 1)
    copy(0, 1, size0)
    array(0) = elem.asInstanceOf[AnyRef]
    size0 += 1
    this
  }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   *  @return      the updated buffer.
   */
  override def ++:(iter: Iterable[A]): this.type = { insertAll(0, iter); this }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def insertAll(n: Int, iter: Iterable[A]) {
    if ((n < 0) || (n > size0))
      throw new IndexOutOfBoundsException(n.toString)
    val xs = iter.elements.toList
    val len = xs.length
    ensureSize(size0 + len)
    copy(n, n + len, size0 - n)
    xs.copyToArray(array.asInstanceOf[scala.Array[Any]], n)
    size0 += len
  }

  /** Removes the element on a given index position. It takes time linear in
   *  the buffer size.
   *
   *  @param n  the index which refers to the first element to delete.
   *  @param count   the number of elemenets to delete
   *  @return   the updated array buffer.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def remove(n: Int, count: Int) {
    if ((n < 0) || (n >= size0))
      throw new IndexOutOfBoundsException(n.toString)
    copy(n + count, n, size0 - (n + count))
    size0 -= count
  }

  /** Return a clone of this buffer.
   *
   *  @return an <code>ArrayBuffer</code> with the same elements.
   */
  override def clone(): Buffer[A] = new ArrayBuffer[A] ++ this

  def result: ArrayBuffer[A] = this

  /** Defines the prefix of the string representation.
   */
  override def stringPrefix: String = "ArrayBuffer"
}
