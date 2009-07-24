/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.generic
import scala.collection._

/** This class represents collections that can be reduced using a - operator.
 *
 *  @author   Martin Odersky
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
  def -(elem: A): This

  /** Returns a new collection that contains all elements of the current collection
   *  except a two or more given elements.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  def -(elem1: A, elem2: A, elems: A*): This =
    this - elem1 - elem2 -- elems

  /** Returns a new collection that contains all elements of the current collection
   *  except the elements provided by a traversable object
   *
   *  @param elems     the traversable object containing the elements that do not form part of the new collection.
   */
  def --(elems: Traversable[A]): This = (thisCollection /: elems) (_ - _)

  /** Returns a new collection that contains all elements of the current collection
   *  except the elements provided by an iterator
   *
   *  @param elems     the iterator containing the elements that do not form part of the new collection
   *  @note  same as --
   */
  def --(iter: Iterator[A]): This = (thisCollection /: iter) (_ - _)
}
