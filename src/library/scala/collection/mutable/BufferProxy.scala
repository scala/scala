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

import script._

/** This is a simple proxy class for <a href="Buffer.html"
 *  target="contentFrame">`scala.collection.mutable.Buffer`</a>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 16/04/2004
 *  @since   1
 *
 *  @tparam A     type of the elements the buffer proxy contains.
 *
 *  @define Coll `BufferProxy`
 *  @define coll buffer proxy
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait BufferProxy[A] extends Buffer[A] with Proxy {

  def self: Buffer[A]

  def length: Int = self.length

  override def iterator: Iterator[A] = self.iterator

  def apply(n: Int): A = self.apply(n)

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): this.type = { self.+=(elem); this }

  /** Appends a number of elements provided by a traversable object.
   *
   *  @param xs   the traversable object.
   *  @return     a reference to this $coll.
   */
  override def ++=(xs: TraversableOnce[A]): this.type = { self.++=(xs); this }

  /** Appends a sequence of elements to this buffer.
   *
   *  @param elems  the elements to append.
   */
  override def append(elems: A*) { self.++=(elems) }

  /** Appends a number of elements provided by a traversable object.
   *
   *  @param xs   the traversable object.
   */
  override def appendAll(xs: TraversableOnce[A]) { self.appendAll(xs) }

  /** Prepend a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   *  @return      a reference to this $coll.
   */
  def +=:(elem: A): this.type = { self.+=:(elem); this }

  override def ++=:(xs: TraversableOnce[A]): this.type = { self.++=:(xs); this }

  /** Prepend an element to this list.
   *
   *  @param elems  the elements to prepend.
   */
  override def prepend(elems: A*) { self.prependAll(elems) }

  /** Prepends a number of elements provided by a traversable object.
   *  The identity of the buffer is returned.
   *
   *  @param xs  the traversable object.
   */
  override def prependAll(xs: TraversableOnce[A]) { self.prependAll(xs) }

  /** Inserts new elements at the index `n`. Opposed to method
   *  `update`, this method will not replace an element with a
   *  one. Instead, it will insert the new elements at index `n`.
   *
   *  @param n      the index where a new element will be inserted.
   *  @param elems  the new elements to insert.
   */
  override def insert(n: Int, elems: A*) { self.insertAll(n, elems) }

  /** Inserts new elements at the index `n`. Opposed to method
   *  `update`, this method will not replace an element with a
   *  one. Instead, it will insert a new element at index `n`.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   */
  def insertAll(n: Int, iter: scala.collection.Iterable[A]) {
    self.insertAll(n, iter)
  }

  override def insertAll(n: Int, iter: scala.collection.Traversable[A]) {
    self.insertAll(n, iter)
  }

  /** Replace element at index `n` with the new element `newelem`.
   *
   *  @param n       the index of the element to replace.
   *  @param newelem the new element.
   */
  def update(n: Int, newelem: A) { self.update(n, newelem) }

  /** Removes the element on a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   */
  def remove(n: Int): A = self.remove(n)

  /** Clears the buffer contents.
   */
  def clear() { self.clear() }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   */
  @deprecated("scripting is deprecated", "2.11.0")
  override def <<(cmd: Message[A]) { self << cmd }

  /** Return a clone of this buffer.
   *
   *  @return a `Buffer` with the same elements.
   */
  override def clone(): Buffer[A] = new BufferProxy[A] {
    def self = BufferProxy.this.self.clone()
  }
}
