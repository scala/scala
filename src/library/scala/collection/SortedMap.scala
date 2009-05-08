/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
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
  override def empty: SortedMap[A, B] = throw new UnsupportedOperationException("SortedMap.empty")
  override protected[this] def newBuilder : Builder[(A, B), SortedMap[A, B], Any] =
    throw new UnsupportedOperationException("SortedMap.newBuilder")

}
