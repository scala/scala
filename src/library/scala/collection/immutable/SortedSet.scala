/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.Builder

/** A sorted set.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.4
 */
trait SortedSet[A] extends Set[A] with scala.collection.SortedSet[A] with SortedSetLike[A, SortedSet[A]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = throw new UnsupportedOperationException("SortedMap.empty")
}

/**
 * @since 2.4
 */
object SortedSet extends ImmutableSortedSetFactory[SortedSet] {
  implicit def builderFactory[A](implicit ord: Ordering[A]): BuilderFactory[A, SortedSet[A], Coll] = new SortedSetBuilderFactory[A]
  def empty[A](implicit ord: Ordering[A]): SortedSet[A] = TreeSet.empty[A]
}
