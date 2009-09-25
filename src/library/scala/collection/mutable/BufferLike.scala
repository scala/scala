/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._
import script._

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
trait BufferLike[A, +This <: BufferLike[A, This] with Buffer[A]]
                extends Growable[A]
                   with Shrinkable[A]
                   with Scriptable[A]
                   with Addable[A, This]
                   with Subtractable[A, This]
                   with Cloneable[This]
                   with SequenceLike[A, This]
{ self =>

  import scala.collection.{Iterable, Traversable}

// Abstract methods from Vector:

  /** Return element at index `n`
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def apply(n: Int): A

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def update(n: Int, newelem: A)

  /** Return number of elements in the buffer
   */
  def length: Int

// Abstract methods from Appendabl

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): this.type

  /** Clears the buffer contents.
   */
  def clear()

// Abstract methods new in this class

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *  @param elem  the element to prepend.
   */
  def +:(elem: A): This

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter    the iterable object providing all elements to insert.
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def insertAll(n: Int, iter: Traversable[A])


  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   *  @return   the previous element
   */
  def remove(n: Int): A

  /** Removes  a number of elements from a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   *  @param count  the number of elements to delete
   *  @throws   IndexOutofBoundsException if the index is not valid
   */
  def remove(n: Int, count: Int) {
    for (i <- 0 until count) remove(n)
  }

  /** Removes a single element from this buffer, at its first occurrence.
   *  If the buffer does not contain that element, it is unchanged.
   *
   *  @param x  the element to remove.
   */
  def -= (x: A): this.type = {
    val i = indexOf(x)
    if (i != -1) remove(i)
    this
  }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.(repr /: elems) (_ plus _)
   *
   *  @param iter  the iterable object.
   */
  def ++:(iter: Traversable[A]): This = { for (x <- iter) x +: this; repr }

  /** Prepends a number of elements provided by an iterator
   *  The identity of the buffer is returned.
   *
   *  @param iter   the iterator
   *  @return       the updated buffer.
   */
  def ++:(iter: Iterator[A]): This = { for (x <- iter) x +: this; repr }

  /** Appends elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  def append(elems: A*) { this ++= elems }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method.
   *
   *  @param iter  the iterable object.
   */
  def appendAll(iter: Traversable[A]) { this ++= iter }

  /** Prepend given elements to this list.
   *
   *  @param elem  the element to prepend.
   */
  def prepend(elems: A*) { elems ++: this }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def prependAll(iter: Traversable[A]) { iter ++: this }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
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
  def insert(n: Int, elems: A*) { insertAll(n, elems) }

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
   */
  def <<(cmd: Message[A]): Unit = cmd match {
    case Include(Start, x)      => prepend(x)
    case Include(End, x)        => append(x)
    case Include(Index(n), x)   => insert(n, x)
    case Include(NoLo, x)       => this += x

    case Update(Start, x)       => update(0, x)
    case Update(End, x)         => update(length - 1, x)
    case Update(Index(n), x)    => update(n, x)

    case Remove(Start, x)       => if (this(0) == x) remove(0)
    case Remove(End, x)         => if (this(length - 1) == x) remove(length - 1)
    case Remove(Index(n), x)    => if (this(n) == x) remove(n)
    case Remove(NoLo, x)        => this -= x

    case Reset()                => clear
    case s: Script[_]           => s.iterator foreach <<
    case _                      => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }

  /** Defines the prefix of the string representation.
   */
  override def stringPrefix: String = "Buffer"

  /** Adds a number of elements in an array
   *
   *  @param src    the array
   *  @param start  the first element to append
   *  @param len    the number of elements to append
   */
  @deprecated("replace by: <code>buf ++= src.view(start, end)</code>")
  def ++=(src: Array[A], start: Int, len: Int) {
    var i = start
    val end = i + len
    while (i < end) {
      this += src(i)
      i += 1
    }
  }

  /** Adds a single element to this collection and returns
   *  the collection itself.
   *
   *  @param elem  the element to add.
   */
  @deprecated("Use += instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=' if you intend to create a new collection.")
  override def + (elem: A): This = { +=(elem); repr }

  /** Adds two or more elements to this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  @deprecated("Use += instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=' if you intend to create a new collection.")
  override def + (elem1: A, elem2: A, elems: A*): This = {
    this += elem1 += elem2 ++= elems
    repr
  }

  /** Adds a number of elements provided by a traversable object and returns
   *  either the collection itself.
   *
   *  @param iter     the iterable object.
   */
  @deprecated("Use ++= instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=` if you intend to create a new collection.")
  override def ++(iter: Traversable[A]): This = {
    for (elem <- iter) +=(elem)
    repr
  }

  /** Adds a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   */
  @deprecated("Use ++= instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=` if you intend to create a new collection.")
  override def ++ (iter: Iterator[A]): This = {
    for (elem <- iter) +=(elem)
    repr
  }

  /** Removes a single element from this collection and returns
   *  the collection itself.
   *
   *  @param elem  the element to remove.
   */
  @deprecated("Use -= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() -=` if you intend to create a new collection.")
  override def -(elem: A): This = { -=(elem); repr }

  /** Removes two or more elements from this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  @deprecated("Use -= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() -=` if you intend to create a new collection.")
  override def -(elem1: A, elem2: A, elems: A*): This = {
    this -= elem1 -= elem2 --= elems
    repr
  }

  /** Removes a number of elements provided by a Traversable object and returns
   *  the collection itself.
   *
   *  @param iter     the Traversable object.
   */
  @deprecated("Use --= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() --=` if you intend to create a new collection.")
  override def --(iter: Traversable[A]): This = {
    for (elem <- iter) -=(elem)
    repr
  }

  /** Removes a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   */
  @deprecated("Use --= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() --=` if you intend to create a new collection.")
  override def --(iter: Iterator[A]): This = {
    for (elem <- iter) -=(elem)
    repr
  }

  def readOnly: scala.collection.Sequence[A] = toSequence
}



