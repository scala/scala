/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package generic

/** This trait represents collection-like objects that can be added to
 *  using a '+' operator. It defines variants of `+` and `++`
 *  as convenience methods in terms of single-element addition `+`.
 *  @tparam   A    the type of the elements of the $coll
 *  @tparam   Repr the type of the $coll itself
 *  @author   Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define  $coll collection
 *  @define  $Coll Addable
 */
trait Addable[A, +Repr <: Addable[A, Repr]] { self =>

  /** The representation object of type `Repr` which contains the collection's elements
   */
  protected def repr: Repr

  /** Creates a new $coll with an additional element, unless the element is already present.
   *  @param elem the element to add
   *  @return a fresh collection with `elem` added.
   */
  def +(elem: A): Repr

  /** Creates a new $coll with additional elements.
   *
   *  This method takes two or more elements to be added. Another overloaded
   *  variant of this method handles the case where a single element is
   *  added.
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return   a new $coll with the given elements added.
   */
  def + (elem1: A, elem2: A, elems: A*): Repr =
    this + elem1 + elem2 ++ elems

  /** Creates a new $coll by adding all elements contained in another collection to this $coll.
   *
   *  @param elems     the collection containing the added elements.
   *  @return a new $coll with the given elements added.
   */
  def ++ (elems: Traversable[A]): Repr = (repr /: elems) (_ + _)

  /** Creates a new $coll by adding all elements produced by an iterator to this $coll.
   *
   *  @param iter     the iterator producing the added elements.
   *  @return a new $coll with the given elements added.
   */
  def ++ (iter: Iterator[A]): Repr = (repr /: iter) (_ + _)
}




