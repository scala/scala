/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


/** A map whose keys are sorted.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
package scala.collection.immutable

import generic._
import annotation.unchecked.uncheckedVariance

trait SortedMap[A, +B] extends Map[A, B]
                         with collection.SortedMap[A, B]
                         with ImmutableMapTemplate[A, B, SortedMap[A, B]]
                         with SortedMapTemplate[A, B, SortedMap[A, B]] {

  /** Needs to be overridden in subclasses. */
  override def empty: SortedMap[A, B] = throw new UnsupportedOperationException("SortedMap.empty")

  /** Needs to be overridden in subclasses. */
  override protected[this] def newBuilder : Builder[(A, B), SortedMap[A, B]] =
    throw new UnsupportedOperationException("SortedMap.newBuilder")

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  def add [B1 >: B](key: A, value: B1): SortedMap[A, B1]

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  override def + [B1 >: B] (kv: (A, B1)): SortedMap[A, B1] = add(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): SortedMap[A, B1] = {
    var m = this + elem1 + elem2;
    for (e <- elems) m = m + e
    m
  }
}

