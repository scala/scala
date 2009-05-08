/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

/** A sorted set.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
package scala.collection.immutable

import generic._

trait SortedSet[A] extends Set[A] with collection.SortedSet[A] with SortedSetTemplate[A, SortedSet[A]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = throw new UnsupportedOperationException("SortedMap.empty")
}
