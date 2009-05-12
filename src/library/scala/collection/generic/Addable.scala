/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $

package scala.collection.generic

/** This class represents collections that can be augmented usmutableing a += operator.
 *
 *  @autor   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Addable[A, +This <: Addable[A, This]] { self =>

  protected def thisCollection: This

  /** Creates a new collection with an additional element, unless the element is already present.
   *  @param elem the element to be added
   *  @return a fresh collection
   */
  def plus(elem: A): This

  /** Creates a new collection with an additional element, unless the element is already present.
   *  @param elem the element to be added
   *  @return a fresh collection
   *
   *  @note  same as `plus`
   */
  def + (elem: A): This = plus(elem)

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def plus (elem1: A, elem2: A, elems: A*): This =
    this plus elem1 plus elem2 plusAll Iterable.fromOld(elems)

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @note  same as `plus`
   */
  def + (elem1: A, elem2: A, elems: A*): This = plus(elem1, elem2, elems: _*)

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  def plusAll(elems: Traversable[A]): This = (thisCollection /: elems) (_ plus _)

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   *  @note  This is a more efficient version of Traversiable.++ which avoids
   *         copying of the collection's elements. However, it applies only if
   *         the type of the added elements is a subtype of the element type of the
   *         collection.
   */
  def ++ (elems: Traversable[A]): This = plusAll(elems)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  def plusAll (iter: Iterator[A]): This = (thisCollection /: iter) (_ plus _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   *  @note  This is a more efficient version of Traversiable.++ which avoids
   *         copying of the collection's elements. However, it applies only if
   *         the type of the added elements is a subtype of the element type of the
   *         collection.
   */
  def ++ (iter: Iterator[A]): This = plusAll(iter)

}




