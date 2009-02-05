/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.generic.mutable

/** This class represents collections that can be augmented using a += operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Growable[A] {

  /** Add a single element to this collection.
   *
   *  @param elem  the element to add.
   */
  def +=(elem: A): Unit

  /** Add a two or more elements to this collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def +=(elem1: A, elem2: A, elems: A*) {
    this += elem1
    this += elem2
    this ++= elems.asInstanceOf[Iterable[A]] // !@!
  }

  /** Adds a number of elements provided by an iterator
   *
   *  @param iter  the iterator.
   */
  def ++=(iter: collection.Iterator[A]) { iter foreach += }

  /** Adds a number of elements provided by an iterable object
   *  via its <code>elements</code> method.
   *
   *  @param iter  the iterable object.
   */
  def ++=(iter: collection.Iterable[A]) { iter foreach += }

  /** Add a single element to this collection and return
   *  the identity of the collection.
   *
   *  @param elem  the element to add.
   */
  def +(elem: A): this.type = { this += elem; this }

  /** Add two or more elements to this collection and return
   *  the identity of the collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def +(elem1: A, elem2: A, elems: A*): this.type =
    this + elem1 + elem2 ++ elems.asInstanceOf[Iterable[A]] // !@!

  /** Adds a number of elements provided by an iterable object
   *  via its <code>elements</code> method. The identity of the
   *  collection is returned.
   *
   *  @param iter     the iterable object.
   *  @return       the updated collection.
   */
  def ++(iter: Iterable[A]): this.type = { this ++= iter; this }

  /** Adds a number of elements provided by an iterator
   *  via its <code>elements</code> method. The identity of the
   *  collection is returned.
   *
   *  @param iter   the iterator
   *  @return       the updated collection.
   */
  def ++(iter: Iterator[A]): this.type = { this ++= iter; this }

  /** Clears the collection contents.
   */
  def clear()
}




