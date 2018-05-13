package scala
package collection

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Builder
import scala.language.higherKinds

import scala.annotation.unchecked.uncheckedVariance

/** Base type of sorted sets */
trait SortedMap[K, +V]
  extends Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]] {

  def unsorted: Map[K, V] = this

  override protected def fromSpecificIterable(coll: Iterable[(K, V)] @uncheckedVariance): SortedMapCC[K, V] @uncheckedVariance = sortedMapFactory.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[(K, V), SortedMapCC[K, V]] @uncheckedVariance = sortedMapFactory.newBuilder[K, V]

  /**
    * @note This operation '''has''' to be overridden by concrete collection classes to effectively
    *       return a `SortedMapFactory[SortedMapCC]`. The implementation in `SortedMap` only returns
    *       a `SortedMapFactory[SortedMap]`, but the compiler will '''not''' throw an error if the
    *       effective `SortedMapCC` type constructor is more specific than `SortedMap`.
    *
    * @return The factory of this collection.
    */
  def sortedMapFactory: SortedMapFactory[SortedMapCC] = SortedMap

  override def empty: SortedMapCC[K, V] @uncheckedVariance = sortedMapFactory.empty
}

trait SortedMapOps[K, +V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C]
     with SortedOps[K, C] {

  /**
    * Type alias to `CC`. It is used to provide a default implementation of the `fromSpecificIterable`
    * and `newSpecificBuilder` operations.
    *
    * Due to the `@uncheckedVariance` annotation, usage of this type member can be unsound and is
    * therefore not recommended.
    */
  protected type SortedMapCC[K, V] = CC[K, V] @uncheckedVariance

  def sortedMapFactory: SortedMapFactory[SortedMapCC]

  /** Similar to `mapFromIterable`, but returns a SortedMap collection type.
    * Note that the return type is now `CC[K2, V2]` aka `SortedMapCC[K2, V2]` rather than `MapCC[(K2, V2)]`.
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
  @SerialVersionUID(3L)
  protected class KeySortedSet extends SortedSet[K] with GenKeySet with GenKeySortedSet {
    def diff(that: Set[K]): SortedSet[K] = fromSpecificIterable(view.filterNot(that))
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

  override def withFilter(p: ((K, V)) => Boolean): SortedMapOps.WithFilter[K, V, IterableCC, MapCC, CC] = new SortedMapOps.WithFilter(this, p)

  // And finally, we add new overloads taking an ordering
  /** Builds a new sorted map by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFactory.from(new View.Map[(K, V), (K2, V2)](toIterable, f))

  /** Builds a new sorted map by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFactory.from(new View.FlatMap(toIterable, f))

  /** Builds a new sorted map by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    flatMap { kv =>
      if (pf.isDefinedAt(kv)) new View.Single(pf(kv))
      else View.Empty
    }

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param xs   the traversable to append.
    *  @tparam K2  the type of the keys of the returned $coll.
    *  @tparam V2  the type of the values of the returned $coll.
    *  @return     a new collection of type `CC[K2, V2]` which contains all elements
    *              of this $coll followed by all elements of `xs`.
    */
  def concat[K2 >: K, V2 >: V](xs: Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] = sortedMapFactory.from(new View.Concat(toIterable, xs))

  /** Alias for `concat` */
  @`inline` final def ++ [K2 >: K, V2 >: V](xs: Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] = concat(xs)

  @deprecated("Consider requiring an immutable SortedMap or fall back to SortedMap.concat ", "2.13.0")
  override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = sortedMapFactory.from(new View.Appended(toIterable, kv))

  // We override these methods to fix their return type (which would be `Map` otherwise)
  override def concat[V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = sortedMapFactory.from(new View.Concat(toIterable, xs))
  override def ++ [V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = concat(xs)
  // TODO Also override mapValues

}

object SortedMapOps {
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

object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap)
