/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic

/** A template for companion objects of immutable.Map and subclasses thereof.
 */
abstract class ImmutableSortedMapFactory[CC[A, B] <: immutable.SortedMap[A, B] with SortedMapTemplate[A, B, CC[A, B]]]
  extends SortedMapFactory[CC] {

  def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), CC[A, B]] =
    new MapBuilder[A, B, CC[A, B]](empty(ord))
}
