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
abstract class ImmutableSortedSetFactory[CC[A] <: immutable.SortedSet[A] with SortedSetTemplate[A, CC[A]]] extends SortedSetFactory[CC]{
  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]] = new SetBuilder[A, CC[A]](empty)
}
