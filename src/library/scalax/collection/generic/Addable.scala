/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scalax.collection.generic

/** This class represents collections that can be augmented using a += operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Addable[+C <: Addable[C, A], A] {

  protected def thisCC: C

  /** Adds a single element to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added element.
   *
   *  @param elem  the element to add.
   */
  def +(elem: A): C

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def +(elem1: A, elem2: A, elems: A*): C =
    thisCC + elem1 + elem2 ++ elems.asInstanceOf[Iterable[A]] // !@!

  /** Adds a number of elements provided by an iterable object
   *  via its <code>elements</code> method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param iter     the iterable object.
   */
  def ++(iter: Iterable[A]): C = (thisCC /: iter) (_ + _)

  /** Adds a number of elements provided by an iterator
   *  via its <code>elements</code> method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param iter   the iterator
   */
  def ++(iter: Iterator[A]): C = (thisCC /: iter) (_ + _)

}




