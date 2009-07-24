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

/** This class represents collections that can be added to other
 *  collections using a '+' operator.
 *
 *  @author   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Addable[A, +This <: Addable[A, This]] { self =>

  protected def thisCollection: This

  /** Creates a new collection with an additional element, unless the element is already present.
   *  @param elem the element to be added
   *  @return a fresh collection
   */
  def +(elem: A): This

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  def + (elem1: A, elem2: A, elems: A*): This =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  def ++ (elems: Traversable[A]): This = (thisCollection /: elems) (_ + _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  def ++ (iter: Iterator[A]): This = (thisCollection /: iter) (_ + _)
}




