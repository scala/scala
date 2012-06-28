/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._

/** A template for maps whose keys are sorted.
 *  To create a concrete sorted map, you need to implement the rangeImpl method,
 *  in addition to those of `MapLike`.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait SortedMapLike[A, +B, +This <: SortedMapLike[A, B, This] with SortedMap[A, B]] extends Sorted[A, This] with MapLike[A, B, This] {
self =>

  def firstKey : A = head._1
  def lastKey : A = last._1

  implicit def ordering: Ordering[A]

  // XXX: implement default version
  def rangeImpl(from : Option[A], until : Option[A]) : This

  override def keySet : SortedSet[A] = new DefaultKeySortedSet

  protected class DefaultKeySortedSet extends super.DefaultKeySet with SortedSet[A] {
    implicit def ordering = self.ordering
    override def + (elem: A): SortedSet[A] = (SortedSet[A]() ++ this + elem)
    override def - (elem: A): SortedSet[A] = (SortedSet[A]() ++ this - elem)
    override def rangeImpl(from : Option[A], until : Option[A]) : SortedSet[A] = {
      val map = self.rangeImpl(from, until)
      new map.DefaultKeySortedSet
    }
  }

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
  
  override def filterKeys(p: A => Boolean): SortedMap[A, B] = new FilteredKeys(p) with SortedMap.Default[A, B] {
    implicit def ordering: Ordering[A] = self.ordering
    override def rangeImpl(from : Option[A], until : Option[A]): SortedMap[A, B] = self.rangeImpl(from, until).filterKeys(p)
  }
  
  override def mapValues[C](f: B => C): SortedMap[A, C] = new MappedValues(f) with SortedMap.Default[A, C] {
    implicit def ordering: Ordering[A] = self.ordering
    override def rangeImpl(from : Option[A], until : Option[A]): SortedMap[A, C] = self.rangeImpl(from, until).mapValues(f)
  }
  
  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs     the traversable object.
   */
  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): SortedMap[A, B1] =
    ((repr: SortedMap[A, B1]) /: xs.seq) (_ + _)
  
}




