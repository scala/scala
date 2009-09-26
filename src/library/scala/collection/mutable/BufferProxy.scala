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

/** This is a simple proxy class for <a href="Buffer.html"
 *  target="contentFrame"><code>scala.collection.mutable.Buffer</code></a>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 16/04/2004
 *  @since   1
 */
trait BufferProxy[A] extends Buffer[A] with Proxy {

  def self: Buffer[A]

  def length: Int = self.length

  override def iterator: Iterator[A] = self.iterator

  def apply(n: Int): A = self.apply(n)

  /** Append a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   *  @return      the updated buffer.
   */
  override def +(elem: A): Buffer[A] = self.+(elem)

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): this.type = { self.+=(elem); this }

  override def readOnly = self.readOnly

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   *  @return      the updated buffer.
   */
  def ++(iter: scala.collection.Iterable[A]): Buffer[A] = self.++(iter)

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method.
   *
   *  @param iter  the iterable object.
   */
  def ++=(iter: scala.collection.Iterable[A]): this.type = { self.++=(iter); this }

  /** Appends a sequence of elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  override def append(elems: A*) { self.++=(elems) }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method.
   *
   *  @param iter  the iterable object.
   */
  def appendAll(iter: scala.collection.Iterable[A]) { self.appendAll(iter) }

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  def +:(elem: A): Buffer[A] = self.+:(elem)

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def ++:(iter: scala.collection.Iterable[A]): Buffer[A] = self.++:(iter)

  /** Prepend an element to this list.
   *
   *  @param elem  the element to prepend.
   */
  override def prepend(elems: A*) { self.prependAll(elems) }

  /** Prepends a number of elements provided by an iterable object
   *  via its <code>iterator</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter  the iterable object.
   */
  def prependAll(elems: scala.collection.Iterable[A]) { self.prependAll(elems) }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert the new elements at index <code>n</code>.
   *
   *  @param n      the index where a new element will be inserted.
   *  @param elems  the new elements to insert.
   */
  override def insert(n: Int, elems: A*) { self.insertAll(n, elems) }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   */
  def insertAll(n: Int, iter: scala.collection.Iterable[A]): Unit = self.insertAll(n, iter)

  override def insertAll(n: Int, iter: scala.collection.Traversable[A]): Unit = self.insertAll(n, iter)

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   */
  def update(n: Int, newelem: A): Unit = self.update(n, newelem)

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   */
  def remove(n: Int): A = self.remove(n)

  /** Clears the buffer contents.
   */
  def clear() { self.clear }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   */
  override def <<(cmd: Message[A]) { self << cmd }

  /** Return a clone of this buffer.
   *
   *  @return a <code>Buffer</code> with the same elements.
   */
  override def clone(): Buffer[A] = new BufferProxy[A] {
    def self = BufferProxy.this.self.clone()
  }
}
