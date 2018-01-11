package strawman
package collection

import strawman.collection.immutable.TreeMap
import strawman.collection.mutable.Builder

import scala.annotation.unchecked.uncheckedVariance
import scala.{Boolean, Int, Option, Ordering, PartialFunction, Serializable, `inline`}

/** Base type of sorted sets */
trait SortedMap[K, +V]
  extends Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]] {
  def unsorted: Map[K, V] = this
}

trait SortedMapOps[K, +V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C]
     with SortedOps[K, C] {

  def sortedMapFactory: SortedMapFactory[CC]

  protected[this] def sortedMapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2]

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
  def minAfter(key: K): Option[(K, V)] = from(key).headOption

  /** Find the element with largest key less than a given key.
    * @param key The given key.
    * @return `None` if there is no such node.
    */
  def maxBefore(key: K): Option[(K, V)] = until(key).lastOption

  def rangeTo(to: K): C = {
    val i = keySet.from(to).iterator()
    if (i.isEmpty) return coll
    val next = i.next()
    if (ordering.compare(next, to) == 0)
      if (i.isEmpty) coll
      else until(i.next())
    else
      until(next)
  }

  override def keySet: SortedSet[K] = new KeySortedSet

  /** The implementation class of the set returned by `keySet` */
  protected class KeySortedSet extends SortedSet[K] with GenKeySet with GenKeySortedSet {
    def iterableFactory: IterableFactory[Set] = Set
    def sortedIterableFactory: SortedIterableFactory[SortedSet] = SortedSet
    protected[this] def fromSpecificIterable(coll: Iterable[K]): SortedSet[K] = sortedFromIterable(coll)
    protected[this] def newSpecificBuilder(): Builder[K, SortedSet[K]] = sortedIterableFactory.newBuilder()
    protected[this] def sortedFromIterable[B: Ordering](it: Iterable[B]): SortedSet[B] = sortedFromIterable(it)
    def diff(that: Set[K]): SortedSet[K] = fromSpecificIterable(view.filterNot(that))
    def empty: SortedSet[K] = sortedIterableFactory.empty
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

  override def withFilter(p: ((K, V)) => Boolean): SortedMapWithFilter = new SortedMapWithFilter(p)

  /** Specializes `MapWithFilter` for sorted Map collections
    *
    * @define coll sorted map collection
    */
  class SortedMapWithFilter(p: ((K, V)) => Boolean) extends MapWithFilter(p) {

    def map[K2 : Ordering, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] =
      sortedMapFactory.from(View.Map(filtered, f))

    def flatMap[K2 : Ordering, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] =
      sortedMapFactory.from(View.FlatMap(filtered, f))

    override def withFilter(q: ((K, V)) => Boolean): SortedMapWithFilter = new SortedMapWithFilter(kv => p(kv) && q(kv))

  }

  // And finally, we add new overloads taking an ordering
  /** Builds a new sorted map by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFromIterable(View.Map[(K, V), (K2, V2)](toIterable, f))

  /** Builds a new sorted map by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *  @param f      the function to apply to each element.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFromIterable(View.FlatMap(toIterable, f))

  /** Builds a new sorted map by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[K2, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    flatMap { (kv: (K, V)) =>
      if (pf.isDefinedAt(kv)) View.Single(pf(kv))
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
  def concat[K2 >: K, V2 >: V](xs: Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] = sortedMapFromIterable(View.Concat(toIterable, xs))

  /** Alias for `concat` */
  @`inline` final def ++ [K2 >: K, V2 >: V](xs: Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] = concat(xs)

  // We override these methods to fix their return type (which would be `Map` otherwise)
  override def concat[V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = sortedMapFromIterable(View.Concat(toIterable, xs))
  override def ++ [V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = concat(xs)
  // TODO Also override mapValues

}

object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap)
