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

/** A Buffer implementation back up by a list. It provides constant time
 *  prepend and append. Most other operations are linear.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @SerialVersionUID(3419063961353022661L)
final class ListBuffer[A]
      extends Buffer[A]
         with GenericTraversableTemplate[A, ListBuffer]
         with BufferLike[A, ListBuffer[A]]
         with Builder[A, List[A]]
         with SeqForwarder[A]
{
  override def companion: GenericCompanion[ListBuffer] = ListBuffer

  import scala.collection.Traversable

  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var exported: Boolean = false
  private var len = 0

  protected def underlying: immutable.Seq[A] = start

  /** The current length of the buffer
   */
  override def length = len

  // Implementations of abstract methods in Buffer

  /** Replaces element at index <code>n</code> with the new element
   *  <code>newelem</code>. Takes time linear in the buffer size. (except the
   *  first element, which is updated in constant time).
   *
   *  @param n  the index of the element to replace.
   *  @param x  the new element.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def update(n: Int, x: A) {
    try {
      if (exported) copy()
      if (n == 0) {
        val newElem = new :: (x, start.tail);
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
    } catch {
      case ex: Exception => throw new IndexOutOfBoundsException(n.toString())
    }
  }

  /** Appends a single element to this buffer. This operation takes constant time.
   *
   *  @param x  the element to append.
   */
  def += (x: A): this.type = {
    if (exported) copy()
    if (start.isEmpty) {
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

  /** Clears the buffer contents.
   */
  def clear() {
    start = Nil
    exported = false
    len = 0
  }

  /** Prepends a single element to this buffer. This operation takes constant
   *  time.
   *
   *  @param x  the element to prepend.
   *  @return   this buffer.
   */
  def +=: (x: A): this.type = {
    if (exported) copy()
    val newElem = new :: (x, start)
    if (start.isEmpty) last0 = newElem
    start = newElem
    len += 1
    this
  }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a new
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param  n     the index where a new element will be inserted.
   *  @param  iter  the iterable object providing all elements to insert.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def insertAll(n: Int, seq: Traversable[A]) {
    try {
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
    } catch {
      case ex: Exception =>
        throw new IndexOutOfBoundsException(n.toString())
    }
  }

  /** Removes a given number of elements on a given index position. May take
   *  time linear in the buffer size.
   *
   *  @param n         the index which refers to the first element to remove.
   *  @param count     the number of elements to remove.
   */
  override def remove(n: Int, count: Int) {
    if (exported) copy()
    val n1 = n max 0
    val count1 = count min (len - n1)
    var old = start.head
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
    len -= count1
  }

// Implementation of abstract method in Builder

  def result: List[A] = toList

  /** Converts this buffer to a list. Takes constant time. The buffer is
   *  copied lazily, the first time it is mutated.
   */
  override def toList: List[A] = {
    exported = !start.isEmpty
    start
  }

// New methods in ListBuffer

  /** Prepends the elements of this buffer to a given list
   *
   *  @param xs   the list to which elements are prepended
   */
  def prependToList(xs: List[A]): List[A] =
    if (start.isEmpty) xs
    else { last0.tl = xs; toList }

// Overrides of methods in Buffer

  /** Removes the element on a given index position. May take time linear in
   *  the buffer size
   *
   *  @param  n  the index which refers to the element to delete.
   *  @return n  the element that was formerly at position <code>n</code>.
   *  @pre       an element exists at position <code>n</code>
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def remove(n: Int): A = try {
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
    len -= 1
    old
  }

  /** Remove a single element from this buffer. May take time linear in the
   *  buffer size.
   *
   *  @param x  the element to remove.
   */
  override def -= (elem: A): this.type = {
    if (exported) copy()
    if (start.isEmpty) {}
    else if (start.head == elem) {
      start = start.tail
      len -= 1
    } else {
      var cursor = start
      while (!cursor.tail.isEmpty && cursor.tail.head != elem) {
        cursor = cursor.tail
      }
      if (!cursor.tail.isEmpty) {
        val z = cursor.asInstanceOf[::[A]]
        if (z.tl == last0)
          last0 = z
        z.tl = cursor.tail.tail
        len -= 1
      }
    }
    this
  }

  override def iterator = new Iterator[A] {
    var cursor: List[A] = null
    def hasNext: Boolean = !start.isEmpty && (cursor ne last0)
    def next(): A =
      if (!hasNext) {
        throw new NoSuchElementException("next on empty Iterator")
      } else {
        if (cursor eq null) cursor = start else cursor = cursor.tail
        cursor.head
      }
  }

  /** expose the underlying list but do not mark it as exported */
  override def readOnly: List[A] = start

  // Private methods

  /** Copy contents of this buffer */
  private def copy() {
    var cursor = start
    val limit = last0.tail
    clear
    while (cursor ne limit) {
      this += cursor.head
      cursor = cursor.tail
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: ListBuffer[_] => this.readOnly equals that.readOnly
    case _                   => super.equals(that)
  }

  /** Returns a clone of this buffer.
   *
   *  @return a <code>ListBuffer</code> with the same elements.
   */
  override def clone(): ListBuffer[A] = (new ListBuffer[A]) ++= this

  /** Defines the prefix of the string representation.
   *
   *  @return the string representation of this buffer.
   */
  override def stringPrefix: String = "ListBuffer"
}

/** Factory object for <code>ListBuffer</code> class.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
object ListBuffer extends SeqFactory[ListBuffer] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ListBuffer[A]] =
    new GenericCanBuildFrom[A] {
      def apply() = newBuilder[A]
    }
  def newBuilder[A]: Builder[A, ListBuffer[A]] =
    new AddingBuilder(new ListBuffer[A])
}
