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
import script._
import scala.annotation.migration

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
 *  @define compatMutate
 *  Note that for backward compatibility reasons, this method
 *  mutates the collection in place, unlike similar but
 *  undeprecated methods throughout the collections hierarchy.
 */
trait BufferLike[A, +This <: BufferLike[A, This] with Buffer[A]]
                extends Growable[A]
                   with Shrinkable[A]
                   with Scriptable[A]
                   with Subtractable[A, This]
                   with SeqLike[A, This]
                   with scala.Cloneable
{ self : This =>

  // Abstract methods from Seq:

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
   *  @throws   IndexOutOfBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length`.
   */
  def insertAll(n: Int, elems: scala.collection.Traversable[A])

   /** Removes the element at a given index from this buffer.
    *
    *  @param n  the index which refers to the element to delete.
    *  @return   the previous element at index `n`
    *   @throws   IndexOutOfBoundsException if the if the index `n` is not in the valid range
    *            `0 <= n < length`.
    */
  def remove(n: Int): A

  /** Removes a number of elements from a given index position.  Subclasses of `BufferLike`
   *  will typically override this method to provide better performance than `count`
   *  successive calls to single-element `remove`.
   *
   *  @param n  the index which refers to the first element to remove.
   *  @param count  the number of elements to remove.
   *  @throws   IndexOutOfBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length - count` (with `count > 0`).
   *  @throws   IllegalArgumentException if `count < 0`.
   */
  def remove(n: Int, count: Int) {
    if (count < 0) throw new IllegalArgumentException("removing negative number of elements: " + count.toString)
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

  /** Prepends elements to this buffer.
   *
   *  @param xs  the TraversableOnce containing the elements to prepend.
   *  @return the buffer itself.
   */
  def ++=:(xs: TraversableOnce[A]): this.type = { insertAll(0, xs.toTraversable); this }

  /** Appends the given elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  def append(elems: A*) { appendAll(elems) }

  /** Appends the elements contained in a traversable object to this buffer.
   *  @param xs  the traversable object containing the elements to append.
   */
  def appendAll(xs: TraversableOnce[A]) { this ++= xs }

  /** Prepends given elements to this buffer.
   *  @param elems  the elements to prepend.
   */
  def prepend(elems: A*) { prependAll(elems) }

  /** Prepends the elements contained in a traversable object to this buffer.
   *  @param xs  the collection containing the elements to prepend.
   */
  def prependAll(xs: TraversableOnce[A]) { xs ++=: this }

  /** Inserts new elements at a given index into this buffer.
   *
   *  @param n      the index where new elements are inserted.
   *  @param elems  the traversable collection containing the elements to insert.
   *  @throws   IndexOutOfBoundsException if the index `n` is not in the valid range
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
  @deprecated("scripting is deprecated", "2.11.0")
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

    case Reset()                => clear()
    case s: Script[_]           => s.iterator foreach <<
    case _                      => throw new UnsupportedOperationException("message " + cmd + " not understood")
  }

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this set.
   *           Unless overridden this is simply `"Buffer"`.
   */
  override def stringPrefix: String = "Buffer"

  /** Creates a new collection containing both the elements of this collection and the provided
   *  traversable object.
   *
   *  @param xs     the traversable object.
   *  @return       a new collection consisting of all the elements of this collection and `xs`.
   */
  @migration("`++` creates a new buffer. Use `++=` to add an element from this buffer and return that buffer itself.", "2.8.0")
  def ++(xs: GenTraversableOnce[A]): This = clone() ++= xs.seq

  /** Creates a new collection with all the elements of this collection except `elem`.
   *
   *  @param elem  the element to remove.
   *  @return      a new collection consisting of all the elements of this collection except `elem`.
   */
  @migration("`-` creates a new buffer. Use `-=` to remove an element from this buffer and return that buffer itself.", "2.8.0")
  override def -(elem: A): This = clone() -= elem

  /** Creates a new collection with all the elements of this collection except the two
   *  or more specified elements.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return      a new collection consisting of all the elements of this collection except
   *               `elem1`, `elem2` and those in `elems`.
   */
  @migration("`-` creates a new buffer. Use `-=` to remove an element from this buffer and return that buffer itself.", "2.8.0")
  override def -(elem1: A, elem2: A, elems: A*): This = clone() -= elem1 -= elem2 --= elems

  /** Creates a new collection with all the elements of this collection except those
   *  provided by the specified traversable object.
   *
   *  @param xs       the traversable object.
   *  @return         a new collection with all the elements of this collection except
   *                  those in `xs`
   */
  @migration("`--` creates a new buffer. Use `--=` to remove an element from this buffer and return that buffer itself.", "2.8.0")
  override def --(xs: GenTraversableOnce[A]): This = clone() --= xs.seq

  /** Return a clone of this buffer.
   *
   *  @return a `Buffer` with the same elements.
   */
  override def clone(): This = {
    val bf = newBuilder
    bf ++= this
    bf.result().asInstanceOf[This]
  }
}
