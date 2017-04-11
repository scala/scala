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
import immutable.{List, Nil, ::}
import java.io.{ObjectOutputStream, ObjectInputStream}

/** A `Buffer` implementation backed by a list. It provides constant time
 *  prepend and append. Most other operations are linear.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#list_buffers "Scala's Collection Library overview"]]
 *  section on `List Buffers` for more information.
 *
 *  @tparam A    the type of this list buffer's elements.
 *
 *  @define Coll `ListBuffer`
 *  @define coll list buffer
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `ListBuffer[B]` because an implicit of type `CanBuildFrom[ListBuffer, B, ListBuffer[B]]`
 *    is defined in object `ListBuffer`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `ListBuffer`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(3419063961353022662L)
final class ListBuffer[A]
      extends AbstractBuffer[A]
         with Buffer[A]
         with GenericTraversableTemplate[A, ListBuffer]
         with BufferLike[A, ListBuffer[A]]
         with ReusableBuilder[A, List[A]]
         with SeqForwarder[A]
         with Serializable
{
  override def companion: GenericCompanion[ListBuffer] = ListBuffer

  import scala.collection.Traversable
  import scala.collection.immutable.ListSerializeEnd

  /** Expected invariants:
   *  If start.isEmpty, last0 == null
   *  If start.nonEmpty, last0 != null
   *  If len == 0, start.isEmpty
   *  If len > 0, start.nonEmpty
   */
  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var exported: Boolean = false
  private var len = 0

  protected def underlying: List[A] = start

  private def writeObject(out: ObjectOutputStream) {
    // write start
    var xs: List[A] = start
    while (!xs.isEmpty) { out.writeObject(xs.head); xs = xs.tail }
    out.writeObject(ListSerializeEnd)

    // no need to write last0

    // write if exported
    out.writeBoolean(exported)

    // write the length
    out.writeInt(len)
  }

  private def readObject(in: ObjectInputStream) {
    // read start, set last0 appropriately
    var elem: A = in.readObject.asInstanceOf[A]
    if (elem == ListSerializeEnd) {
      start = Nil
      last0 = null
    } else {
      var current = new ::(elem, Nil)
      start = current
      elem = in.readObject.asInstanceOf[A]
      while (elem != ListSerializeEnd) {
        val list = new ::(elem, Nil)
        current.tl = list
        current = list
        elem = in.readObject.asInstanceOf[A]
      }
      last0 = current
      start
    }

    // read if exported
    exported = in.readBoolean()

    // read the length
    len = in.readInt()
  }

  /** The current length of the buffer.
   *
   *  This operation takes constant time.
   */
  override def length = len

  // Don't use the inherited size, which forwards to a List and is O(n).
  override def size = length

  // Override with efficient implementations using the extra size information available to ListBuffer.
  override def isEmpty: Boolean = len == 0
  override def nonEmpty: Boolean = len > 0

  // Implementations of abstract methods in Buffer

  override def apply(n: Int): A =
    if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString())
    else super.apply(n)

  /** Replaces element at index `n` with the new element
   *  `newelem`. Takes time linear in the buffer size. (except the
   *  first element, which is updated in constant time).
   *
   *  @param n  the index of the element to replace.
   *  @param x  the new element.
   *  @throws IndexOutOfBoundsException if `n` is out of bounds.
   */
  def update(n: Int, x: A) {
    // We check the bounds early, so that we don't trigger copying.
    if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString)
    if (exported) copy()
    if (n == 0) {
      val newElem = new :: (x, start.tail)
      if (last0 eq start) {
        last0 = newElem
      }
      start = newElem
    } else {
      var cursor = start
      var i = 1
      while (i < n) {
        cursor = cursor.tail
        i += 1
      }
      val newElem = new :: (x, cursor.tail.tail)
      if (last0 eq cursor.tail) {
        last0 = newElem
      }
      cursor.asInstanceOf[::[A]].tl = newElem
    }
  }

  /** Appends a single element to this buffer. This operation takes constant time.
   *
   *  @param x  the element to append.
   *  @return   this $coll.
   */
  def += (x: A): this.type = {
    if (exported) copy()
    if (isEmpty) {
      last0 = new :: (x, Nil)
      start = last0
    } else {
      val last1 = last0
      last0 = new :: (x, Nil)
      last1.tl = last0
    }
    len += 1
    this
  }

  override def ++=(xs: TraversableOnce[A]): this.type = xs match {
    case x: AnyRef if x eq this      => this ++= (this take size)
    case _                           => super.++=(xs)

  }

  override def ++=:(xs: TraversableOnce[A]): this.type =
    if (xs.asInstanceOf[AnyRef] eq this) ++=: (this take size) else super.++=:(xs)

  /** Clears the buffer contents.
   */
  def clear() {
    start = Nil
    last0 = null
    exported = false
    len = 0
  }

  /** Prepends a single element to this buffer. This operation takes constant
   *  time.
   *
   *  @param x  the element to prepend.
   *  @return   this $coll.
   */
  def +=: (x: A): this.type = {
    if (exported) copy()
    val newElem = new :: (x, start)
    if (isEmpty) last0 = newElem
    start = newElem
    len += 1
    this
  }

  /** Inserts new elements at the index `n`. Opposed to method
   *  `update`, this method will not replace an element with a new
   *  one. Instead, it will insert a new element at index `n`.
   *
   *  @param  n     the index where a new element will be inserted.
   *  @param  seq   the iterable object providing all elements to insert.
   *  @throws IndexOutOfBoundsException if `n` is out of bounds.
   */
  def insertAll(n: Int, seq: Traversable[A]) {
    // We check the bounds early, so that we don't trigger copying.
    if (n < 0 || n > len) throw new IndexOutOfBoundsException(n.toString)
    if (exported) copy()
    var elems = seq.toList.reverse
    len += elems.length
    if (n == 0) {
      while (!elems.isEmpty) {
        val newElem = new :: (elems.head, start)
        if (start.isEmpty) last0 = newElem
        start = newElem
        elems = elems.tail
      }
    } else {
      var cursor = start
      var i = 1
      while (i < n) {
        cursor = cursor.tail
        i += 1
      }
      while (!elems.isEmpty) {
        val newElem = new :: (elems.head, cursor.tail)
        if (cursor.tail.isEmpty) last0 = newElem
        cursor.asInstanceOf[::[A]].tl = newElem
        elems = elems.tail
      }
    }
  }

  /** Reduce the length of the buffer, and null out last0
   *  if this reduces the length to 0.
   */
  private def reduceLengthBy(num: Int) {
    len -= num
    if (len <= 0)   // obviously shouldn't be < 0, but still better not to leak
      last0 = null
  }

  /** Removes a given number of elements on a given index position. May take
   *  time linear in the buffer size.
   *
   *  @param n         the index which refers to the first element to remove.
   *  @param count     the number of elements to remove.
   *  @throws   IndexOutOfBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length - count` (with `count > 0`).
   *  @throws   IllegalArgumentException if `count < 0`.
   */
  override def remove(n: Int, count: Int) {
    if (count < 0) throw new IllegalArgumentException("removing negative number of elements: " + count.toString)
    else if (count == 0) return  // Nothing to do
    if (n < 0 || n > len - count) throw new IndexOutOfBoundsException("at " + n.toString + " deleting " + count.toString)
    if (exported) copy()
    val n1 = n max 0
    val count1 = count min (len - n1)
    if (n1 == 0) {
      var c = count1
      while (c > 0) {
        start = start.tail
        c -= 1
      }
    } else {
      var cursor = start
      var i = 1
      while (i < n1) {
        cursor = cursor.tail
        i += 1
      }
      var c = count1
      while (c > 0) {
        if (last0 eq cursor.tail) last0 = cursor.asInstanceOf[::[A]]
        cursor.asInstanceOf[::[A]].tl = cursor.tail.tail
        c -= 1
      }
    }
    reduceLengthBy(count1)
  }

// Implementation of abstract method in Builder

  /** Returns the accumulated `List`.
   *
   *  This method may be called multiple times to obtain snapshots of the list in different stages of construction.
   */
  def result: List[A] = toList

  /** Converts this buffer to a list. Takes constant time. The buffer is
   *  copied lazily, the first time it is mutated.
   */
  override def toList: List[A] = {
    exported = !isEmpty
    start
  }

// New methods in ListBuffer

  /** Prepends the elements of this buffer to a given list
   *
   *  @param xs   the list to which elements are prepended
   */
  def prependToList(xs: List[A]): List[A] = {
    if (isEmpty) xs
    else {
      if (exported) copy()
      last0.tl = xs
      toList
    }
  }

// Overrides of methods in Buffer

  /** Removes the element on a given index position. May take time linear in
   *  the buffer size.
   *
   *  @param  n  the index which refers to the element to delete.
   *  @return n  the element that was formerly at position `n`.
   *  @note      an element must exists at position `n`.
   *  @throws IndexOutOfBoundsException if `n` is out of bounds.
   */
  def remove(n: Int): A = {
    if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString())
    if (exported) copy()
    var old = start.head
    if (n == 0) {
      start = start.tail
    } else {
      var cursor = start
      var i = 1
      while (i < n) {
        cursor = cursor.tail
        i += 1
      }
      old = cursor.tail.head
      if (last0 eq cursor.tail) last0 = cursor.asInstanceOf[::[A]]
      cursor.asInstanceOf[::[A]].tl = cursor.tail.tail
    }
    reduceLengthBy(1)
    old
  }

  /** Remove a single element from this buffer. May take time linear in the
   *  buffer size.
   *
   *  @param elem  the element to remove.
   *  @return      this $coll.
   */
  override def -= (elem: A): this.type = {
    if (exported) copy()
    if (isEmpty) {}
    else if (start.head == elem) {
      start = start.tail
      reduceLengthBy(1)
    }
    else {
      var cursor = start
      while (!cursor.tail.isEmpty && cursor.tail.head != elem) {
        cursor = cursor.tail
      }
      if (!cursor.tail.isEmpty) {
        val z = cursor.asInstanceOf[::[A]]
        if (z.tl == last0)
          last0 = z
        z.tl = cursor.tail.tail
        reduceLengthBy(1)
      }
    }
    this
  }

  /** Selects the last element.
   *
   *  Runs in constant time.
   *
   *  @return the last element of this buffer.
   *  @throws NoSuchElementException if this buffer is empty.
   */
  override def last: A =
    if (last0 eq null) throw new NoSuchElementException("last of empty ListBuffer")
    else last0.head

  /** Optionally selects the last element.
   *
   *  Runs in constant time.
   *
   *  @return `Some` of the last element of this buffer if the buffer is nonempty, `None` if it is empty.
   */
  override def lastOption: Option[A] = if (last0 eq null) None else Some(last0.head)

  /** Returns an iterator over this `ListBuffer`.  The iterator will reflect
   *  changes made to the underlying `ListBuffer` beyond the next element;
   *  the next element's value is cached so that `hasNext` and `next` are
   *  guaranteed to be consistent.  In particular, an empty `ListBuffer`
   *  will give an empty iterator even if the `ListBuffer` is later filled.
   */
  override def iterator: Iterator[A] = new AbstractIterator[A] {
    // Have to be careful iterating over mutable structures.
    // This used to have "(cursor ne last0)" as part of its hasNext
    // condition, which means it can return true even when the iterator
    // is exhausted.  Inconsistent results are acceptable when one mutates
    // a structure while iterating, but we should never return hasNext == true
    // on exhausted iterators (thus creating exceptions) merely because
    // values were changed in-place.
    var cursor: List[A] = if (ListBuffer.this.isEmpty) Nil else start

    def hasNext: Boolean = cursor ne Nil
    def next(): A =
      if (!hasNext) throw new NoSuchElementException("next on empty Iterator")
      else {
        val ans = cursor.head
        cursor = cursor.tail
        ans
      }
  }

  // Private methods

  /** Copy contents of this buffer */
  private def copy() {
    if (isEmpty) return
    var cursor = start
    val limit = last0.tail
    clear()
    while (cursor ne limit) {
      this += cursor.head
      cursor = cursor.tail
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: ListBuffer[_] => this.start equals that.start
    case _                   => super.equals(that)
  }

  /** Returns a clone of this buffer.
   *
   *  @return a `ListBuffer` with the same elements.
   */
  override def clone(): ListBuffer[A] = (new ListBuffer[A]) ++= this

  /** Defines the prefix of the string representation.
   *
   *  @return the string representation of this buffer.
   */
  override def stringPrefix: String = "ListBuffer"
}

/** $factoryInfo
 *  @define Coll `ListBuffer`
 *  @define coll list buffer
 */
object ListBuffer extends SeqFactory[ListBuffer] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ListBuffer[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, ListBuffer[A]] = new GrowingBuilder(new ListBuffer[A])
}
