/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._
import script._
import annotation.migration

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
                   with Subtractable[A, This]
                   with Cloneable[This]
                   with SeqLike[A, This]
{ self : This =>

  // Note this does not extend Addable because `+` is being phased out of
  // all Seq-derived classes.

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
   *  @param elems  the collection containing the elements to prepend.
   */
  def prependAll(xs: TraversableOnce[A]) { xs ++=: this }

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

  /** Provide a read-only view of this buffer as a sequence
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

  @deprecated("use `+=:' instead")
  final def +:(elem: A): This = +=:(elem)

  /** Adds a single element to this collection and returns
   *  the collection itself.  Note that for backward compatibility
   *  reasons, this method mutates the collection in place, unlike
   *  similar but undeprecated methods throughout the collections
   *  hierarchy.  You are strongly recommended to use '+=' instead.
   *
   *  @param elem  the element to add.
   */
  @deprecated("Use += instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() +=' if you intend to create a new collection.")
  def + (elem: A): This = { +=(elem); repr }

  /** Adds two or more elements to this collection and returns
   *  the collection itself.  Note that for backward compatibility
   *  reasons, this method mutates the collection in place, unlike
   *  all similar methods throughout the collections hierarchy.
   *  similar but undeprecated methods throughout the collections
   *  hierarchy.  You are strongly recommended to use '++=' instead.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  @deprecated("Use ++= instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=' if you intend to create a new collection.")
  def + (elem1: A, elem2: A, elems: A*): This = {
    this += elem1 += elem2 ++= elems
    repr
  }

  /** Creates a new collection containing both the elements of this collection and the provided
   *  traversable object.
   *
   *  @param xs     the traversable object.
   *  @return       a new collection consisting of all the elements of this collection and `xs`.
   */
  @migration(2, 8,
    "As of 2.8, ++ always creates a new collection, even on Buffers.\n"+
    "Use ++= instead if you intend to add by side effect to an existing collection.\n"
  )
  def ++(xs: TraversableOnce[A]): This = clone() ++= xs

  /** Creates a new collection with all the elements of this collection except `elem`.
   *
   *  @param elem  the element to remove.
   *  @return      a new collection consisting of all the elements of this collection except `elem`.
   */
  @migration(2, 8,
    "As of 2.8, - always creates a new collection, even on Buffers.\n"+
    "Use -= instead if you intend to remove by side effect from an existing collection.\n"
  )
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
  @migration(2, 8,
    "As of 2.8, - always creates a new collection, even on Buffers.\n"+
    "Use -= instead if you intend to remove by side effect from an existing collection.\n"
  )
  override def -(elem1: A, elem2: A, elems: A*): This = clone() -= elem1 -= elem2 --= elems

  /** Creates a new collection with all the elements of this collection except those
   *  provided by the specified traversable object.
   *
   *  @param xs       the traversable object.
   *  @return         a new collection with all the elements of this collection except
   *                  those in `xs`
   */
  @migration(2, 8,
    "As of 2.8, -- always creates a new collection, even on Buffers.\n"+
    "Use --= instead if you intend to remove by side effect from an existing collection.\n"
  )
  override def --(xs: TraversableOnce[A]): This = clone() --= xs
}
