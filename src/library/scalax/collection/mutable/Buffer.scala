/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Buffer.scala 15799 2008-08-15 18:23:54Z odersky $


package scalax.collection.mutable

import generic.mutable.VectorTemplate

/** Buffers are used to create sequences of elements incrementally by
 *  appending, prepending, or inserting new elements. It is also
 *  possible to access and modify elements in a random access fashion
 *  via the index of the element in the current sequence.
  *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
  */
@cloneable
trait Buffer[A] extends Vector[A] with VectorTemplate[Buffer, A] with Appendable[A]
//      with Scriptable[Message[(Location, A)]]
      with CloneableCollection
{

// Abstract methods from Vector:

  /** Return element at index `n`
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def apply(n: Int): A

  /** Return number of elements in the buffer
   */
  def length: Int

  /** Create a new buffer of the same kind as this one */
  def newBuilder[B]: Builder[Buffer, B] = new ArrayBuffer[B]

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def update(n: Int, newelem: A): Unit

// Abstract methods from Appendable

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): Unit

  /** Clears the buffer contents.
   */
  def clear()

// Abstract methods new in this class

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *  @param elem  the element to prepend.
   */
  def +:(elem: A): this.type

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter    the iterable object providing all elements to insert.
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def insertAll(n: Int, iter: Iterable[A]): Unit

  /** Removes  a number of elements from a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   *  @param count  the number of elements to delete
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def remove(n: Int, count: Int): Unit

// Concrete methods

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   */
  def remove(n: Int): A = {
    val elem = this(n)
    remove(n, 1)
    elem
  }

  /** Removes a single element from this buffer, at its first occurrence.
   *  If the list does not contain that element, it is unchanged
   *
   *  @param x  the element to remove.
   */
  def -= (x: A) {
    val i = indexOf(x)
    if (i != -1) remove(i)
  }

  /** Removes a single element from this buffer, at its first occurrence,
   *  and returns the identity of the buffer.
   *  If the buffer does not contain that element, it is unchanged
   *
   *  @param x  the element to remove.
   */
  def - (x: A): this.type = { -=(x); this }

  /** Prepend two ore more elements to this buffer and return
   *  the identity of the buffer.
   *  @param elem  the element to prepend.
   */
  def +:(elem1: A, elem2: A, elems: A*): this.type =
    elem1 +: elem2 +: (elems.asInstanceOf[Iterable[A]]) ++: this // !!!

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def ++:(iter: Iterable[A]): this.type = { for (x <- iter) x +: this; this }

  /** Prepends a number of elements provided by an iterator
   *  The identity of the buffer is returned.
   *
   *  @param iter   the iterator
   *  @return       the updated buffer.
   */
  def ++:(iter: Iterator[A]): this.type = { for (x <- iter) x +: this; this }

  /** Appends a elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  def append(elems: A*) { this ++= elems.asInstanceOf[Iterable[A]] } // !!!

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method.
   *
   *  @param iter  the iterable object.
   */
  def appendAll(iter: Iterable[A]) { this ++= iter }

  /** Prepend given elements to this list.
   *
   *  @param elem  the element to prepend.
   */
  def prepend(elems: A*) { elems.asInstanceOf[Iterable[A]] ++: this } // !!!

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def prependAll(iter: Iterable[A]) { iter ++: this }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def prependAll(iter: Iterator[A]) { iter ++: this }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert the new elements at index <code>n</code>.
   *
   *  @param n      the index where a new element will be inserted.
   *  @param elems  the new elements to insert.
   */
  def insert(n: Int, elems: A*) { insertAll(n, elems.asInstanceOf[Iterable[A]]) } // !!!

  /** Removes the first <code>n</code> elements.
   *
   *  @param n  the number of elements to remove from the beginning
   *            of this buffer.
   */
  def trimStart(n: Int) { remove(0, n) }

  /** Removes the last <code>n</code> elements.
   *
   *  @param n  the number of elements to remove from the end
   *            of this buffer.
   */
  def trimEnd(n: Int) { remove(length - n max 0, n) }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   *  !!!! todo: rewrite location, msg etc with enumerations or else pack in a subpackage
  def <<(cmd: Message[(Location, A)]) {
    cmd match {
      case Include((l, elem)) => l match {
        case Start => prepend(elem)
        case End => append(elem)
        case Index(n) => insert(n, elem)
        case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
      }
      case Update((l, elem)) => l match {
        case Start => update(0, elem)
        case End => update(length - 1, elem)
        case Index(n) => update(n, elem)
        case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
      }
      case Remove((l, _)) => l match {
        case Start => remove(0)
        case End => remove(length - 1)
        case Index(n) => remove(n)
        case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
      }
      case Reset() => clear
      case s: Script[_] => s.elements foreach <<
      case _ => throw new UnsupportedOperationException("message " + cmd + " not understood")
    }
  }
   */

  /** Return a clone of this buffer.
   *
   *  @return a buffer with the same elements.
   */
  override def clone(): Buffer[A] = super.clone().asInstanceOf[Buffer[A]]

  /** Defines the prefix of the string representation.
   */
  override def stringPrefix: String = "Buffer"

  /** Adds a number of elements in an array
   *
   *  @param src    the array
   *  @param start  the first element to append
   *  @param len    the number of elements to append
   *  @deprecated   replace by: <code>buf ++= src.view(start, end)</code>
   */
  @deprecated def ++=(src: Array[A], start: Int, len: Int) {
    var i = start
    val end = i + len
    while (i < end) {
      this += src(i)
      i += 1
    }
  }

}




