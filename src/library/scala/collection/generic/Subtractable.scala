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

  /** Returns a new collection that contains all elements of the current collection
   *  except a given element.
   *
   *  @param elem  the element to remove.
   */
  def minus(elem: A): This

  /** Returns a new collection that contains all elements of the current collection
   *  except a given element.
   *
   *  @param elem  the element to remove.
   *  @note  same as `minus`
   */
  def -(elem: A): This = minus(elem)

  /** Returns a new collection that contains all elements of the current collection
   *  except a two or more given elements.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  def minus(elem1: A, elem2: A, elems: A*): This =
    this minus elem1 minus elem2 minusAll Iterable.fromOld(elems)

  /** Returns a new collection that contains all elements of the current collection
   *  except two or more given elements.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @note  same as minus
   */
  def - (elem1: A, elem2: A, elems: A*): This = minus(elem1, elem2, elems: _*)

  /** Returns a new collection that contains all elements of the current collection
   *  except the elements provided by a traversable object
   *
   *  @param elems     the traversable object containing the elements that do not form part of the new collection.
   */
  def minusAll(elems: Traversable[A]): This = (thisCollection /: elems) (_ minus _)

  /** Returns a new collection that contains all elements of the current collection
   *  except the elements provided by a traversable object
   *
   *  @param elems     the traversable object containing the elements that do not form part of the new collection.
   *  @note  same as minusAll
   */
  def --(elems: Traversable[A]): This = minusAll(elems)

  /** Returns a new collection that contains all elements of the current collection
   *  except the elements provided by an iterator
   *
   *  @param elems     the iterator containing the elements that do not form part of the new collection
   *  @note  same as minusAll
   */
  def minusAll(iter: Iterator[A]): This = (thisCollection /: iter) (_ minus _)

  /** Returns a new collection that contains all elements of the current collection
   *  except the elements provided by an iterator
   *
   *  @param elems     the iterator containing the elements that do not form part of the new collection
   *  @note  same as minusAll
   */
  def --(iter: Iterator[A]): This = minusAll(iter)
}
