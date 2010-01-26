/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._
import script._

/** A template trait for buffers of type `Buffer[A]`.
 *
 *  Buffers are used to create sequences of elements incrementally by
 *  appending, prepending, or inserting new elements. It is also
 *  possible to access and modify elements in a random access fashion
 *  via the index of the element in the current sequence.
 *
 *  @tparam A    the type of the elements of the buffer
 *  @tparam This the type of the buffer itself.
 *
 *  $buffernote
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 2.8
 *  @since   2.8
 *  @define buffernote @note
 *    This trait provides most of the operations of a `Buffer` independently of its representation.
 *    It is typically inherited by concrete implementations of buffers.
 *
 *    To implement a concrete buffer, you need to provide implementations of the
 *    following methods:
 *    {{{
 *       def apply(idx: Int): A
 *       def update(idx: Int, elem: A)
 *       def length: Int
 *       def clear()
 *       def +=(elem: A): this.type
 *       def +=:(elem: A): this.type
 *       def insertAll(n: Int, iter: Traversable[A])
 *       def remove(n: Int): A
 *    }}}
 *  @define coll buffer
 *  @define Coll Buffer
 *  @define add  append
 *  @define Add  Append
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
@cloneable
trait BufferLike[A, +This <: BufferLike[A, This] with Buffer[A]]
                extends Growable[A]
                   with Shrinkable[A]
                   with Scriptable[A]
                   with Addable[A, This]
                   with Subtractable[A, This]
                   with Cloneable[This]
                   with SeqLike[A, This]
{ self : This =>

  import scala.collection.{Iterable, Traversable}

  // Abstract methods from IndexedSeq:

  def apply(n: Int): A
  def update(n: Int, newelem: A)
  def length: Int

  // Abstract methods from Growable:

  def +=(elem: A): this.type
  def clear()

  // Abstract methods new in this class:

  /** Prepends a single element to this buffer.
   *  @param elem  the element to prepend.
   *  @return      the buffer itself.
   */
  def +=:(elem: A): this.type

  /** Inserts new elements at a given index into this buffer.
   *
   *  @param n      the index where new elements are inserted.
   *  @param elems  the traversable collection containing the elements to insert.
   *  @throws   IndexOutofBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length`.
   */
  def insertAll(n: Int, elems: Traversable[A])

   /** Removes the element at a given index from this buffer.
    *
    *  @param n  the index which refers to the element to delete.
    *  @return   the previous element at index `n`
    *   @throws   IndexOutofBoundsException if the if the index `n` is not in the valid range
    *            `0 <= n < length`.
    */
  def remove(n: Int): A

  /** Removes a number of elements from a given index position.
   *
   *  @param n  the index which refers to the first element to remove.
   *  @param count  the number of elements to remove.
   *  @throws   IndexOutofBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length - count`.
   *  @throws   IllegalArgumentException if `count < 0`.
   */
  def remove(n: Int, count: Int) {
    for (i <- 0 until count) remove(n)
  }

  /** Removes a single element from this buffer, at its first occurrence.
   *  If the buffer does not contain that element, it is unchanged.
   *
   *  @param x  the element to remove.
   *  @return   the buffer itself
   */
  def -= (x: A): this.type = {
    val i = indexOf(x)
    if (i != -1) remove(i)
    this
  }

  /** Prepends the elements contained in a traversable collection
   *  to this buffer.
   *  @param elems  the collection containing the elements to prepend.
   *  @return the buffer itself.
   */
  def ++=:(elems: Traversable[A]): this.type = { insertAll(0, elems); this }

  /** Prepends the elements produced by an iterator to this buffer.
   *
   *  @param iter   the iterator producing the elements to prepend.
   *  @return       the buffer itself.
   */
  def ++=:(iter: Iterator[A]): this.type = { insertAll(0, iter.toSeq); this }

  /** Appends the given elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  def append(elems: A*) { this ++= elems }

  /** Appends the elements contained in a traversable collection to this buffer.
   *  @param elems  the collection containing the elements to append.
   */
  def appendAll(elems: Traversable[A]) { this ++= elems }

  /** Appends the elements produced by an iterator to this buffer.
   *  @param elems  the iterator producing the elements to append.
   */
  def appendAll(iter: Iterator[A]) { this ++= iter }

  /** Prepends given elements to this buffer.
   *  @param elems  the elements to prepend.
   */
  def prepend(elems: A*) { elems ++=: this }

  /** Prepends the elements contained in a traversable collection to this buffer.
   *  @param elems  the collection containing the elements to prepend.
   */
  def prependAll(iter: Traversable[A]) { iter ++=: this }

  /** Prepends a number of elements produced by an iterator to this buffer.
   *  @param iter  the iterator producing the elements to prepend.
   */
  def prependAll(iter: Iterator[A]) { iter ++=: this }

  /** Inserts new elements at a given index into this buffer.
   *
   *  @param n      the index where new elements are inserted.
   *  @param elems  the traversable collection containing the elements to insert.
   *  @throws   IndexOutofBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length`.
   */
  def insert(n: Int, elems: A*) { insertAll(n, elems) }

  /** Removes the first ''n'' elements of this buffer.
   *
   *  @param n  the number of elements to remove from the beginning
   *            of this buffer.
   */
  def trimStart(n: Int) { remove(0, n) }

  /** Removes the last ''n'' elements of this buffer.
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

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this set.
   *           Unless overridden this is simply `"Buffer"`.
   */
  override def stringPrefix: String = "Buffer"

  /** Provide a read-only view of this byffer as a sequence
   *  @return  A sequence which refers to this buffer for all its operations.
   */
  def readOnly: scala.collection.Seq[A] = toSeq

  /** Adds a number of elements in an array
   *
   *  @param src    the array
   *  @param start  the first element to append
   *  @param len    the number of elements to append
   */
  @deprecated("replace by: `buf ++= src.view(start, end)`")
  def ++=(src: Array[A], start: Int, len: Int) {
    var i = start
    val end = i + len
    while (i < end) {
      this += src(i)
      i += 1
    }
  }

  @deprecated("use ++=: instead")
  final def ++:(iter: Traversable[A]): This = ++=:(iter)

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

  @deprecated("use `+=:' instead")
  final def +:(elem: A): This = +=:(elem)

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
}



