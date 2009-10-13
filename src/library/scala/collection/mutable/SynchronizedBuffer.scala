/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import script._

/** This class should be used as a mixin. It synchronizes the <code>Buffer</code>
 *  methods of the class into which it is mixed in.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   1
 */
trait SynchronizedBuffer[A] extends Buffer[A] {

  import scala.collection.Traversable

  abstract override def length: Int = synchronized {
    super.length
  }

  abstract override def iterator: Iterator[A] = synchronized {
    super.iterator
  }

  abstract override def apply(n: Int): A = synchronized {
    super.apply(n)
  }

  /** Append a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  override def +(elem: A): Buffer[A] = synchronized {
    super.+(elem)
  }

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  abstract override def +=(elem: A): this.type = synchronized[this.type] {
    super.+=(elem)
  }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  override def ++(iter: Traversable[A]): Buffer[A] = synchronized {
    super.++(iter)
  }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method.
   *
   *  @param iter  the iterable object.
   */
  override def ++=(iter: Traversable[A]): this.type = synchronized[this.type] {
    super.++=(iter)
  }

  /** Appends a sequence of elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  override def append(elems: A*): Unit = synchronized {
    super.++=(elems)
  }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method.
   *
   *  @param iter  the iterable object.
   */
  override def appendAll(iter: Traversable[A]): Unit = synchronized {
    super.appendAll(iter)
  }

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  abstract override def +=:(elem: A): Buffer[A] = synchronized {
    super.+=:(elem)
  }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  override def ++:(iter: Traversable[A]): Buffer[A] = synchronized {
    super.++:(iter)
  }

  /** Prepend an element to this list.
   *
   *  @param elem  the element to prepend.
   */
  override def prepend(elems: A*): Unit = synchronized {
    super.prependAll(elems)
  }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  override def prependAll(elems: Traversable[A]): Unit = synchronized {
    super.prependAll(elems)
  }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert the new elements at index <code>n</code>.
   *
   *  @param n      the index where a new element will be inserted.
   *  @param elems  the new elements to insert.
   */
  override def insert(n: Int, elems: A*): Unit = synchronized {
    super.insertAll(n, elems)
  }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   */
  abstract override def insertAll(n: Int, iter: Traversable[A]): Unit = synchronized {
     super.insertAll(n, iter)
  }

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   */
  abstract override def update(n: Int, newelem: A): Unit = synchronized {
    super.update(n, newelem)
  }

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   */
  abstract override def remove(n: Int): A = synchronized {
    super.remove(n)
  }

  /** Clears the buffer contents.
   */
  abstract override def clear(): Unit = synchronized {
    super.clear
  }

  override def <<(cmd: Message[A]): Unit = synchronized {
    super.<<(cmd)
  }

  /** Return a clone of this buffer.
   *
   *  @return an <code>ArrayBuffer</code> with the same elements.
   */
  override def clone(): Buffer[A] = synchronized {
    super.clone()
  }

  /** The hashCode method always yields an error, since it is not
   *  safe to use buffers as keys in hash tables.
   *
   *  @return never.
   */
  override def hashCode(): Int = synchronized {
    super.hashCode()
  }
}
