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

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class SortedMapFactory[CC[A, B] <: SortedMap[A, B] with SortedMapTemplate[A, B, CC[A, B]]] {

  type Coll = CC[_, _]

  def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), CC[A, B]]

  def empty[A, B](implicit ord: Ordering[A]): CC[A, B]

  def apply[A, B](elems: (A, B)*)(implicit ord: Ordering[A]): CC[A, B] = (newBuilder[A, B](ord) ++= elems).result

  class SortedMapBuilderFactory[A, B](implicit ord: Ordering[A]) extends BuilderFactory[(A, B), CC[A, B], Coll] {
    def apply(from: Coll) = newBuilder[A, B](ord)
  }
}
