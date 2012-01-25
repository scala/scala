/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._
import immutable.{List, Nil, ::}
import java.io._

/** A `Buffer` implementation back up by a list. It provides constant time
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
 *  @define Coll ListBuffer
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
         with Builder[A, List[A]]
         with SeqForwarder[A]
         with Serializable
{
  override def companion: GenericCompanion[ListBuffer] = ListBuffer

  import scala.collection.Traversable
  import scala.collection.immutable.ListSerializeEnd

  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var exported: Boolean = false
  private var len = 0

  protected def underlying: immutable.Seq[A] = start
  
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
   *  @throws Predef.IndexOutOfBoundsException if `n` is out of bounds.
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
   *  @return   this $coll.
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

  override def ++=(xs: TraversableOnce[A]): this.type =
    if (xs eq this) ++= (this take size) else super.++=(xs)

  override def ++=:(xs: TraversableOnce[A]): this.type =
    if (xs eq this) ++=: (this take size) else super.++=:(xs)

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
   *  @return   this $coll.
   */
  def +=: (x: A): this.type = {
    if (exported) copy()
    val newElem = new :: (x, start)
    if (start.isEmpty) last0 = newElem
    start = newElem
    len += 1
    this
  }

  /** Inserts new elements at the index `n`. Opposed to method
   *  `update`, this method will not replace an element with a new
   *  one. Instead, it will insert a new element at index `n`.
   *
   *  @param  n     the index where a new element will be inserted.
   *  @param  iter  the iterable object providing all elements to insert.
   *  @throws Predef.IndexOutOfBoundsException if `n` is out of bounds.
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
  def prependToList(xs: List[A]): List[A] = {
    if (start.isEmpty) xs
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
   *  @throws Predef.IndexOutOfBoundsException if `n` is out of bounds.
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
    len -= 1
    old
  }

  /** Remove a single element from this buffer. May take time linear in the
   *  buffer size.
   *
   *  @param x  the element to remove.
   *  @return   this $coll.
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

  override def iterator: Iterator[A] = new AbstractIterator[A] {
    // Have to be careful iterating over mutable structures.
    // This used to have "(cursor ne last0)" as part of its hasNext
    // condition, which means it can return true even when the iterator
    // is exhausted.  Inconsistent results are acceptable when one mutates
    // a structure while iterating, but we should never return hasNext == true
    // on exhausted iterators (thus creating exceptions) merely because
    // values were changed in-place.
    var cursor: List[A] = null
    var delivered = 0

    // Note: arguably this should not be a "dynamic test" against
    // the present length of the buffer, but fixed at the size of the
    // buffer when the iterator is created.  At the moment such a
    // change breaks tests: see comment on def units in Global.scala.
    def hasNext: Boolean = delivered < ListBuffer.this.length
    def next(): A =
      if (!hasNext)
        throw new NoSuchElementException("next on empty Iterator")
      else {
        if (cursor eq null) cursor = start
        else cursor = cursor.tail
        delivered += 1
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
    clear()
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
 *  @define Coll ListBuffer
 *  @define coll list buffer
 */
object ListBuffer extends SeqFactory[ListBuffer] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ListBuffer[A]] = ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]
  def newBuilder[A]: Builder[A, ListBuffer[A]] = new GrowingBuilder(new ListBuffer[A])
}
