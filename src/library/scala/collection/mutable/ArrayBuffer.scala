/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


import Predef._

/** An implementation of the <code>Buffer</code> class using an array to
 *  represent the assembled sequence internally.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 15/03/2004
 */
@serializable
class ArrayBuffer[A] extends Buffer[A] with ResizableArray[A] {

  /** Appends a single element to this buffer and returns
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): Unit = {
    ensureSize(size+1)
    array(size) = elem
    size = size + 1
  }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   *  @return      the updated buffer.
   */
  override def ++=(iter: Iterable[A]): Unit = iter copyToBuffer this

  /** Appends a number of elements in an array
   *
   *  @param src    the array
   *  @param start  the first element to append
   *  @param len    the number of elements to append
   */
  override def ++=(src: Array[A], start: int, len: int): Unit = {
    ensureSize(size + len)
    Array.copy(src, start, array, size, len)
    size = size + len
  }

  /** Prepends a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   *  @return      the updated buffer.
   */
  def +:(elem: A): Buffer[A] = {
    ensureSize(size+1)
    copy(0, 1, size)
    array(0) = elem
    size = size + 1
    this
  }

  /** Returns the i-th element of this ArrayBuffer.
   *
   *  @param i  the specified index.
   *  @return   the i-th element.
   *  @throws Predef.IndexOutOfBoundException if <code>i</code> is out of bounds.
   */
  override def apply(i: Int) = {
    if ((i < 0) || (i >= size))
      throw new IndexOutOfBoundsException(i.toString())
    else
      array(i)
  }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   *  @return      the updated buffer.
   */
  override def ++:(iter: Iterable[A]): Buffer[A] = { insertAll(0, iter); this }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def insertAll(n: Int, iter: Iterable[A]): Unit = {
    if ((n < 0) || (n > size))
      throw new IndexOutOfBoundsException("cannot insert element at " + n);
    val xs = iter.elements.toList
    val len = xs.length
    ensureSize(size+len)
    copy(n, n + len, size - n)
    xs.copyToArray(array, n)
    size = size + len
  }

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def update(n: Int, newelem: A): Unit = {
    if ((n < 0) || (n >= size))
      throw new IndexOutOfBoundsException("cannot update element at " + n);
    else {
      val res = array(n)
      array(n) = newelem
      res
    }
  }

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   *  @return   the updated array buffer.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def remove(n: Int): A = {
    if ((n < 0) || (n >= size))
      throw new IndexOutOfBoundsException("cannot remove element at " + n);
    val res = array(n);
    copy(n + 1, n, size - n - 1);
    size = size - 1;
    res
  }

  /** Clears the buffer contents.
   */
  def clear(): Unit = {
    size = 0
  }

  /** Return a clone of this buffer.
   *
   *  @return an <code>ArrayBuffer</code> with the same elements.
   */
  override def clone(): Buffer[A] = {
    val res = new ArrayBuffer[A]
    res ++= this
    res
  }

  /** Checks if two buffers are structurally identical.
   *
   *  @return true, iff both buffers contain the same sequence of elements.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: ArrayBuffer[_] =>
      this.length == that.length &&
    elements.zip(that.elements).forall {
      case (thiselem, thatelem) => thiselem == thatelem
    }
    case _ =>
      false
  }

  /** Defines the prefix of the string representation.
   */
  override protected def stringPrefix: String = "ArrayBuffer"
}
