/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: SortedMap.scala 16893 2009-01-13 13:09:22Z cunei $
package scala.collection.generic

/** A template for maps whose keys are sorted.
 *  To create a concrete sorted map, you need to implement the rangeImpl method,
 *  in addition to those of `MapTemplate`.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
trait SortedMapTemplate[A, +B, +This <: SortedMapTemplate[A, B, This] with SortedMap[A, B]] extends Sorted[A, This] with MapTemplate[A, B, This] {
self =>

  def firstKey : A = head._1
  def lastKey : A = last._1

  implicit def ordering: Ordering[A]

  // XXX: implement default version
  def rangeImpl(from : Option[A], until : Option[A]) : This

  protected class DefaultKeySet extends super.DefaultKeySet with SortedSet[A] {
    def compare(k0: A, k1: A) = self.thisCollection.compare(k0, k1)
    /** We can't give an implementation of +/- here because we do not have a generic sorted set implementation
     */
    override def + (elem: A): SortedSet[A] = throw new UnsupportedOperationException("keySet.+")
    override def - (elem: A): SortedSet[A] = throw new UnsupportedOperationException("keySet.-")
    override def rangeImpl(from : Option[A], until : Option[A]) : SortedSet[A] = {
      val map = self.rangeImpl(from, until)
      new map.DefaultKeySet
    }
  }

  override def keySet : SortedSet[A] = new DefaultKeySet

  /** Add a key/value pair to this map.
   *  @param    key the key
   *  @param    value the value
   *  @return   A new map with the new binding added to this map
   */
  override def updated[B1 >: B](key: A, value: B1): SortedMap[A, B1] = this+((key, value))

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  def + [B1 >: B] (kv: (A, B1)): SortedMap[A, B1]

  // todo: Add generic +,-, and so on.

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
