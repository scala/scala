/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.Builder
import annotation.unchecked.uncheckedVariance

/** A map whose keys are sorted.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.4
 */
trait SortedMap[A, +B] extends Map[A, B]
                         with scala.collection.SortedMap[A, B]
                         with MapLike[A, B, SortedMap[A, B]]
                         with SortedMapLike[A, B, SortedMap[A, B]] {

  override protected[this] def newBuilder : Builder[(A, B), SortedMap[A, B]] =
    SortedMap.newBuilder[A, B]

  override def empty: SortedMap[A, B] = SortedMap.empty

  override def updated [B1 >: B](key: A, value: B1): SortedMap[A, B1] = this + ((key, value))

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   *  @note     needs to be overridden in subclasses
   */
  def + [B1 >: B](kv: (A, B1)): SortedMap[A, B1] = throw new AbstractMethodError("SortedMap.+")

  /** Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): SortedMap[A, B1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param elems     the traversable object.
   */
  override def ++[B1 >: B](elems: scala.collection.Traversable[(A, B1)]): SortedMap[A, B1] =
    ((repr: SortedMap[A, B1]) /: elems) (_ + _)

  /** Adds a number of elements provided by an iterator
   *  and returns a new collection with the added elements.
   *
   *  @param iter   the iterator
   */
  override def ++[B1 >: B] (iter: Iterator[(A, B1)]): SortedMap[A, B1] =
    ((repr: SortedMap[A, B1]) /: iter) (_ + _)
}

/**
 * @since 2.4
 */
object SortedMap extends ImmutableSortedMapFactory[SortedMap] {
  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), SortedMap[A, B]] = new SortedMapCanBuildFrom[A, B]
  def empty[A, B](implicit ord: Ordering[A]): SortedMap[A, B] = TreeMap.empty[A, B]
}
