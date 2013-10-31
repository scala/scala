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

/** This trait forms part of collections that can be reduced
 *  using a `-=` operator.
 *
 *  @author   Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @define coll shrinkable collection
 *  @define Coll `Shrinkable`
 */
trait Shrinkable[-A] {

  /** Removes a single element from this $coll.
   *
   *  @param elem  the element to remove.
   *  @return the $coll itself
   */
  def -=(elem: A): this.type

  /** Removes two or more elements from this $coll.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return the $coll itself
   */
  def -=(elem1: A, elem2: A, elems: A*): this.type = {
    this -= elem1
    this -= elem2
    this --= elems
  }

  /** Removes all elements produced by an iterator from this $coll.
   *
   *  @param xs   the iterator producing the elements to remove.
   *  @return the $coll itself
   */
  def --=(xs: TraversableOnce[A]): this.type = { xs foreach -= ; this }
}
