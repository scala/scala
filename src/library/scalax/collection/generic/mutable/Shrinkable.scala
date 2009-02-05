/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.generic.mutable

/** This class represents collections that can be shrunk using a -= operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Shrinkable[A] {

  /** Remove a single element from this collection.
   *
   *  @param elem  the element to append.
   */
  def -=(elem: A): Unit

  /** Remove two or more elements from this collection.
   *  @param    elem1 the first element.
   *  @param    elem2 the second element.
   *  @param    elems the remaining elements.
   */
  def -= (elem1: A, elem2: A, elems: A*) {
    this -= elem1
    this -= elem2
    this --= elems.asInstanceOf[Iterable[A]] // !@!
  }

  /** Remove all the elements provided by an iterable
   *  <code>elems</code> from the collection.
   */
  def --=(elems: Iterable[A]) { elems foreach -= }

  /** Remove all the elements provided by an iterator
   *  <code>elems</code> from the collection.
   */
  def --=(elems: Iterator[A]) { elems foreach -= }
  /** Remove a single element from the collection.
   *  @return the collection itself with the element removed.
   *
   *  @param elem the element to be removed
   */
  def - (elem: A): this.type = { -=(elem); this }

  /** Remove two or more elements from this collection.
   *
   *  @param    elem1 the first element.
   *  @param    elem2 the second element.
   *  @param    elems the remaining elements.
   *  @return the collection itself with the elements removed.
   */
  def - (elem1: A, elem2: A, elems: A*): this.type = { -=(elem1, elem2, elems: _*); this }

  /** Remove all the elements provided by an iterator
   *  <code>elems</code> from the collection.
   *
   *  @param elems An iterator containing the elements to remove from the collection.
   *  @return the collection itself with the elements removed.
   */
  def -- (elems: Iterable[A]): this.type = { this --= elems; this }

  /** Remove all the elements provided by an iterator
   *  <code>elems</code> from the collection.
   *
   *  @param elems An iterator containing the elements to remove from the collection.
   *  @return the collection itself with the elements removed.
   */
  def -- (elems: Iterator[A]): this.type = { this --= elems; this }
}




