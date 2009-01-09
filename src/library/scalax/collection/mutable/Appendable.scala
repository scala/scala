/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.mutable

/** This class represents collections that can be augmented using a += operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Appendable[A] {

  /** Append a single element to this buffer.
   *
   *  @param elem  the element to append.
   */
  def +=(elem: A): Unit

  /** Append a two or more elements to this buffer.
   *
   *  @param elem1 the first element to append.
   *  @param elem2 the second element to append.
   *  @param elems the remaining elements to append.
   */
  def +=(elem1: A, elem2: A, elems: A*) {
    this += elem1
    this += elem2
    this ++= elems.asInstanceOf[Iterable[A]] // !@!
  }

  /** Appends a number of elements provided by an iterator
   *
   *  @param iter  the iterator.
   */
  def ++=(iter: collection.Iterator[A]) { iter foreach += }

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method.
   *
   *  @param iter  the iterable object.
   */
  def ++=(iter: collection.Iterable[A]) { iter foreach += }

  /** Append a single element to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem  the element to append.
   */
  def +(elem: A): this.type = { this += elem; this }

  /** Append two or more elements to this buffer and return
   *  the identity of the buffer.
   *
   *  @param elem1 the first element to append.
   *  @param elem2 the second element to append.
   *  @param elems the remaining elements to append.
   */
  def +(elem1: A, elem2: A, elems: A*): this.type =
    this + elem1 + elem2 ++ elems.asInstanceOf[Iterable[A]] // !@!

  /** Appends a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter     the iterable object.
   *  @return       the updated buffer.
   */
  def ++(iter: Iterable[A]): this.type = { this ++= iter; this }

  /** Appends a number of elements provided by an iterator
   *  via its <code>elements</code> method. The identity of the
   *  buffer is returned.
   *
   *  @param iter   the iterator
   *  @return       the updated buffer.
   */
  def ++(iter: Iterator[A]): this.type = { this ++= iter; this }

  /** Clears the buffer contents.
   */
  def clear()
}




