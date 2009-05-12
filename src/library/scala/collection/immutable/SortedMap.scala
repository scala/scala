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
  def updated [B1 >: B](key: A, value: B1): SortedMap[A, B1]

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  override def plus [B1 >: B] (kv: (A, B1)): SortedMap[A, B1] = updated(kv._1, kv._2)

  /** Add a key/value pair to this map, returning a new map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   *  @note  same as `plus`
   */
  override def + [B1 >: B] (kv: (A, B1)): SortedMap[A, B1] = updated(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def plus [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): SortedMap[A, B1] =
    this plus elem1 plus elem2 plusAll elems

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): SortedMap[A, B1] =
    plus(elem1, elem2, elems: _*)

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  override def plusAll[B1 >: B](elems: collection.Traversable[(A, B1)]): SortedMap[A, B1] =
    ((thisCollection: SortedMap[A, B1]) /: elems) (_ plus _)

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   *  @note  same as `plusAll`
   *  @note  This is a more efficient version of Traversable.++ which avoids
   *         copying of the collection's elements. However, it applies only if
   *         the type of the added elements is a subtype of the element type of the
   *         collection.
   */
  override def ++ [B1 >: B](elems: collection.Traversable[(A, B1)]): SortedMap[A, B1] = plusAll(elems)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  override def plusAll[B1 >: B] (iter: Iterator[(A, B1)]): SortedMap[A, B1] =
    ((thisCollection: SortedMap[A, B1]) /: iter) (_ plus _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   *  @note  same as `plusAll`
   *  @note  This is a more efficient version of Traversable.++ which avoids
   *         copying of the collection's elements. However, it applies only if
   *         the type of the added elements is a subtype of the element type of the
   *         collection.
   */
  override def ++ [B1 >: B](iter: Iterator[(A, B1)]): SortedMap[A, B1] = plusAll(iter)
}

