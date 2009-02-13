/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListBuffer.scala 14378 2008-03-13 11:39:05Z dragos $


package scalax.collection.mutable

import generic._
import immutable.List
import collection.immutable.{List, Nil, ::}

/* Factory object for `ListBuffer` class */
object ListBuffer extends SequenceFactory[ListBuffer] {
  def apply[A](args: A*): ListBuffer[A] = new ListBuffer[A]
}

/** A Buffer implementation back up by a list. It provides constant time
 *  prepend and append. Most other operations are linear.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
@serializable
final class ListBuffer[A]
      extends Buffer[A]
         with SequenceTemplate[ListBuffer, A]
         with Addable[ListBuffer[A], A]
         with Subtractable[ListBuffer[A], A]
         with Builder[List, A]
         with SequenceForwarder[A]
{
  import collection.immutable.Sequence

  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var exported: Boolean = false

  protected def underlying: Sequence[A] = start

  // Implementations of abstract methods in Buffer

  /** Replaces element at index <code>n</code> with the new element
   *  <code>newelem</code>. Takes time linear in the buffer size. (except the first
   *  element, which is updated in constant time).
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
        if (last0 eq start) last0 = newElem
        start = newElem
      } else {
        var cursor = start
        var i = 1
        while (i < n) {
          cursor = cursor.tail
          i += 1
        }
        val newElem = new :: (x, cursor.tail.tail)
        if (last0 eq cursor.tail) last0 = newElem
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
  def += (x: A) {
    if (exported) copy()
    if (start.isEmpty) {
      last0 = new :: (x, Nil)
      start = last0
    } else {
      val last1 = last0
      last0 = new :: (x, Nil)
      last1.tl = last0
    }
  }

  /** Clears the buffer contents.
   */
  def clear() {
    start = Nil
    exported = false
  }

  /** Prepends a single element to this buffer. This operation takes constant
   *  time.
   *
   *  @param x  the element to prepend.
   *  @return   this buffer.
   */
  def +: (x: A): this.type = {
    if (exported) copy()
    val newElem = new :: (x, start)
    if (start.isEmpty) last0 = newElem
    start = newElem
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
  def insertAll(n: Int, iter: Iterable[A]) {
    try {
      if (exported) copy()
      var elems = iter.elements.toList.reverse
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

  /** Removes the element on a given index position. This operation takes time linear in
   *  the buffer size.
   *
   *  @param n  the index which refers to the element to delete.
   *  @return   the updated array buffer.
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  def remove(n: Int, count: Int) {
    try {
      if (exported) copy()
      var old = start.head;
      if (n == 0) {
        var c = count
        while (c > 0) {
          start = start.tail
          c -= 1
        }
      } else {
        var cursor = start
        var i = 1
        while (i < n) {
          cursor = cursor.tail
          i += 1
        }
        var c = count
        while (c > 0) {
          if (last0 eq cursor.tail) last0 = cursor.asInstanceOf[::[A]]
          cursor.asInstanceOf[::[A]].tl = cursor.tail.tail
          c -= 1
        }
      }
      old
    } catch {
      case ex: Exception =>
        throw new IndexOutOfBoundsException(n.toString())
    }  }

// Implementation of abstract method in Builder

  def result = toList

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

  /** Removes the element on a given index position. Takes time linear in
   *  the buffer size (except for the first element, which is removed in constant
   *  time).
   *
   *  @param  n  the index which refers to the element to delete.
   *  @return n  the element that was formerly at position <code>n</code>.
   *  @pre       an element exists at position <code>n</code>
   *  @throws Predef.IndexOutOfBoundsException if <code>n</code> is out of bounds.
   */
  override def remove(n: Int): A = try {
    if (exported) copy()
    var old = start.head;
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
    old
  } catch {
    case ex: Exception =>
      throw new IndexOutOfBoundsException(n.toString())
  }

  /** Remove a single element from this buffer. This operation takes linear time
   *  (except removing the first element, which is done  in constant time).
   *
   *  @param x  the element to remove.
   */
  override def -= (x: A) {
    if (exported) copy()
    if (start.isEmpty) {}
    else if (start.head == x) start = start.tail
    else {
      var cursor = start
      while (!cursor.tail.isEmpty && cursor.tail.head != x) { cursor = cursor.tail }
      if (!cursor.tail.isEmpty) {
        val z = cursor.asInstanceOf[::[A]]
        if (z.tl == last0)
          last0 = z
        z.tl = cursor.tail.tail
      }
    }
  }

  /** expose the underlying list but do not mark it as exported */
  def readOnly: List[A] = start

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

  /** Returns a clone of this buffer.
   *
   *  @return a <code>ListBuffer</code> with the same elements.
   */
  override def clone(): Buffer[A] = (new ListBuffer[A]) ++ this

  /** Defines the prefix of the string representation.
   *
   *  @return the string representation of this buffer.
   */
  override def stringPrefix: String = "ListBuffer"
}

