/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package collection
package generic


/** This trait represents collection-like objects that can be reduced
 *  using a '+' operator. It defines variants of `-` and `--`
 *  as convenience methods in terms of single-element removal `-`.
 *  @tparam   A    the type of the elements of the $coll.
 *  @tparam   Repr the type of the $coll itself
 *  @author   Martin Odersky
 *  @version  2.8
 *  @since    2.8
 *  @define   coll collection
 *  @define   Coll Subtractable
 */
trait Subtractable[A, +Repr <: Subtractable[A, Repr]] { self =>

  /** The representation object of type `Repr` which contains the collection's elements
   */
  protected def repr: Repr

  /** Creates a new $coll from this $coll with an element removed.
   *  @param elem the element to remove
   *  @return a new collection that contains all elements of the current $coll
   *  except one less occurrence of `elem`.
   */
  def -(elem: A): Repr

  /** Creates a new $coll from this $coll with some elements removed.
   *
   *  This method takes two or more elements to be removed. Another overloaded
   *  variant of this method handles the case where a single element is
   *  removed.
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return a new $coll that contains all elements of the current $coll
   *  except one less occurrence of each of the given elements.
   */
  def -(elem1: A, elem2: A, elems: A*): Repr =
    this - elem1 - elem2 -- elems

  /** Creates a new $coll from this $coll by removing all elements of another
   *  collection.
   *
   *  @param xs     the collection containing the removed elements.
   *  @return a new $coll that contains all elements of the current $coll
   *  except one less occurrence of each of the elements of `elems`.
   */
  def --(xs: GenTraversableOnce[A]): Repr = (repr /: xs.seq) (_ - _)
}
