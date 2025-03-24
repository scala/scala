/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.annotation.{implicitNotFound, nowarn}

/** A Map whose keys are sorted according to a [[scala.math.Ordering]]*/
trait SortedMap[K, +V]
  extends Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]
    with SortedMapFactoryDefaults[K, V, SortedMap, Iterable, Map]{

  def unsorted: Map[K, V] = this

  def sortedMapFactory: SortedMapFactory[SortedMap] = SortedMap

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix: String = "SortedMap"

  override def equals(that: Any): Boolean = that match {
    case _ if this eq that.asInstanceOf[AnyRef] => true
    case sm: SortedMap[K @unchecked, _] if sm.ordering == this.ordering =>
      (sm canEqual this) &&
        (this.size == sm.size) && {
        val i1 = this.iterator
        val i2 = sm.iterator
        var allEqual = true
        while (allEqual && i1.hasNext) {
          val kv1 = i1.next()
          val kv2 = i2.next()
          allEqual = ordering.equiv(kv1._1, kv2._1) && kv1._2 == kv2._2
        }
        allEqual
      }
    case _ => super.equals(that)
  }
}

trait SortedMapOps[K, +V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C]
     with SortedOps[K, C] {

  /** The companion object of this sorted map, providing various factory methods.
    *
    * @note When implementing a custom collection type and refining `CC` to the new type, this
    *       method needs to be overridden to return a factory for the new type (the compiler will
    *       issue an error otherwise).
    */
  def sortedMapFactory: SortedMapFactory[CC]

  /** Similar to `mapFromIterable`, but returns a SortedMap collection type.
    * Note that the return type is now `CC[K2, V2]`.
    */
  @`inline` protected final def sortedMapFromIterable[K2, V2](it: Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] = sortedMapFactory.from(it)

  def unsorted: Map[K, V]

  /**
    * Creates an iterator over all the key/value pairs
    * contained in this map having a key greater than or
    * equal to `start` according to the ordering of
    * this map. x.iteratorFrom(y) is equivalent
    * to but often more efficient than x.from(y).iterator.
    *
    * @param start The lower bound (inclusive)
    * on the keys to be returned
    */
  def iteratorFrom(start: K): Iterator[(K, V)]

  /**
    * Creates an iterator over all the keys(or elements)  contained in this
    * collection greater than or equal to `start`
    * according to the ordering of this collection. x.keysIteratorFrom(y)
    * is equivalent to but often more efficient than
    * x.from(y).keysIterator.
    *
    * @param start The lower bound (inclusive)
    * on the keys to be returned
    */
  def keysIteratorFrom(start: K): Iterator[K]

  /**
    * Creates an iterator over all the values contained in this
    * map that are associated with a key greater than or equal to `start`
    * according to the ordering of this map. x.valuesIteratorFrom(y) is
    * equivalent to but often more efficient than
    * x.from(y).valuesIterator.
    *
    * @param start The lower bound (inclusive)
    * on the keys to be returned
    */
  def valuesIteratorFrom(start: K): Iterator[V] = iteratorFrom(start).map(_._2)

  def firstKey: K = head._1
  def lastKey: K = last._1

  /** Find the element with smallest key larger than or equal to a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def minAfter(key: K): Option[(K, V)] = rangeFrom(key).headOption

  /** Find the element with largest key less than a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def maxBefore(key: K): Option[(K, V)] = rangeUntil(key).lastOption

  def rangeTo(to: K): C = {
    val i = keySet.rangeFrom(to).iterator
    if (i.isEmpty) return coll
    val next = i.next()
    if (ordering.compare(next, to) == 0)
      if (i.isEmpty) coll
      else rangeUntil(i.next())
    else
      rangeUntil(next)
  }

  override def keySet: SortedSet[K] = new KeySortedSet

  /** The implementation class of the set returned by `keySet` */
  protected class KeySortedSet extends SortedSet[K] with GenKeySet with GenKeySortedSet {
    def diff(that: Set[K]): SortedSet[K] = fromSpecific(view.filterNot(that))
    def rangeImpl(from: Option[K], until: Option[K]): SortedSet[K] = {
      val map = SortedMapOps.this.rangeImpl(from, until)
      new map.KeySortedSet
    }
  }

  /** A generic trait that is reused by sorted keyset implementations */
  protected trait GenKeySortedSet extends GenKeySet { this: SortedSet[K] =>
    implicit def ordering: Ordering[K] = SortedMapOps.this.ordering
    def iteratorFrom(start: K): Iterator[K] = SortedMapOps.this.keysIteratorFrom(start)
  }

  // And finally, we add new overloads taking an ordering
  /** Builds a new sorted map by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFactory.from(new View.Map[(K, V), (K2, V2)](this, f))

  /** Builds a new sorted map by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFactory.from(new View.FlatMap(this, f))

  /** Builds a new sorted map by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit @implicitNotFound(SortedMapOps.ordMsg) ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFactory.from(new View.Collect(this, pf))

  override def concat[V2 >: V](suffix: IterableOnce[(K, V2)]): CC[K, V2] = sortedMapFactory.from(suffix match {
    case it: Iterable[(K, V2)] => new View.Concat(this, it)
    case _ => iterator.concat(suffix.iterator)
  })(using ordering)

  /** Alias for `concat` */
  @`inline` override final def ++ [V2 >: V](xs: IterableOnce[(K, V2)]): CC[K, V2] = concat(xs)

  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = sortedMapFactory.from(new View.Appended(this, kv))(using ordering)

  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): CC[K, V1] = sortedMapFactory.from(new View.Concat(new View.Appended(new View.Appended(this, elem1), elem2), elems))(using ordering)
}

object SortedMapOps {
  private[collection] final val ordMsg = "No implicit Ordering[${K2}] found to build a SortedMap[${K2}, ${V2}]. You may want to upcast to a Map[${K}, ${V}] first by calling `unsorted`."

  /** Specializes `MapWithFilter` for sorted Map collections
    *
    * @define coll sorted map collection
    */
  class WithFilter[K, +V, +IterableCC[_], +MapCC[X, Y] <: Map[X, Y], +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _]](
    self: SortedMapOps[K, V, CC, _] with MapOps[K, V, MapCC, _] with IterableOps[(K, V), IterableCC, _],
    p: ((K, V)) => Boolean
  ) extends MapOps.WithFilter[K, V, IterableCC, MapCC](self, p) {

    def map[K2 : Ordering, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] =
      self.sortedMapFactory.from(new View.Map(filtered, f))

    def flatMap[K2 : Ordering, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] =
      self.sortedMapFactory.from(new View.FlatMap(filtered, f))

    override def withFilter(q: ((K, V)) => Boolean): WithFilter[K, V, IterableCC, MapCC, CC] =
      new WithFilter[K, V, IterableCC, MapCC, CC](self, (kv: (K, V)) => p(kv) && q(kv))

  }

}

@SerialVersionUID(3L)
object SortedMap extends SortedMapFactory.Delegate[SortedMap](immutable.SortedMap)
