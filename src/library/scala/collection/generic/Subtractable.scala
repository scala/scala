/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scala.collection.generic

/** This class represents collections that can be reduced using a -= operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Subtractable[A, +This <: Subtractable[A, This]] { self =>

  protected def thisCollection: This

  /** Removes a single element from this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the removed element.
   *
   *  @param elem  the element to remove.
   */
  def -(elem: A): This

  /** Removes two or more elements from this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the removed elements.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  def -(elem1: A, elem2: A, elems: A*): This =
    this - elem1 - elem2 -- Iterable.fromOld(elems)

  /** Removes a number of elements provided by an iterable object
   *  via its <code>elements</code> method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the removed elements.
   *
   *  @param iter     the iterable object.
   */
  def --(iter: Traversable[A]): This = (thisCollection /: iter) (_ - _)

  /** Removes a number of elements provided by an iterator
   *  via its <code>elements</code> method and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the removed elements.
   *
   *  @param iter   the iterator
   */
  def --(iter: Iterator[A]): This = (thisCollection /: iter) (_ - _)

}




