/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$
package scala.collection

import generic._

/** A map whose keys are sorted.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
trait SortedMap[A, +B] extends Map[A, B] with SortedMapTemplate[A, B, SortedMap[A, B]] {
  /** Needs to be overridden in subclasses. */
  override def empty = SortedMap.empty[A, B]

  override protected[this] def newBuilder : Builder[(A, B), SortedMap[A, B]] =
    immutable.SortedMap.newBuilder[A, B]
}

object SortedMap extends ImmutableSortedMapFactory[immutable.SortedMap] {
  implicit def builderFactory[A, B](implicit ord: Ordering[A]): BuilderFactory[(A, B), SortedMap[A, B], Coll] = new SortedMapBuilderFactory[A, B]
  def empty[A, B](implicit ord: Ordering[A]): immutable.SortedMap[A, B] = immutable.SortedMap.empty[A, B](ord)
}



