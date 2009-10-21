/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

import mutable.Builder

/** A template for companion objects of mutable.Map and subclasses thereof.
 *
 *  @since 2.8
 */
abstract class SortedMapFactory[CC[A, B] <: SortedMap[A, B] with SortedMapLike[A, B, CC[A, B]]] {

  type Coll = CC[_, _]

  def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), CC[A, B]]

  def empty[A, B](implicit ord: Ordering[A]): CC[A, B]

  def apply[A, B](elems: (A, B)*)(implicit ord: Ordering[A]): CC[A, B] = (newBuilder[A, B](ord) ++= elems).result

  class SortedMapCanBuildFrom[A, B](implicit ord: Ordering[A]) extends CanBuildFrom[Coll, (A, B), CC[A, B]] {
    def apply(from: Coll) = newBuilder[A, B](ord)
    def apply() = newBuilder[A, B]
  }
}
