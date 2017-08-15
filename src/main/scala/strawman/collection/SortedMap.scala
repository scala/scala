package strawman
package collection

import strawman.collection.immutable.TreeMap

import scala.annotation.unchecked.uncheckedVariance
import scala.{Boolean, Ordering, PartialFunction, `inline`}

/** Base type of sorted sets */
trait SortedMap[K, +V]
  extends Map[K, V]
    with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]

trait SortedMapOps[K, +V, +CC[X, Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C]
     with SortedOps[K, C] {

  def sortedMapFactory: SortedMapFactory[CC]

  protected[this] def sortedMapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2]

  def firstKey: K = head._1
  def lastKey: K = last._1

  override def withFilter(p: ((K, V)) => Boolean): SortedMapWithFilter = new SortedMapWithFilter(p)

  /** Specializes `MapWithFilter` for sorted Map collections */
  class SortedMapWithFilter(p: ((K, V)) => Boolean) extends MapWithFilter(p) {

    def map[K2 : Ordering, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2] =
      sortedMapFactory.sortedFromIterable(View.Map(filtered, f))

    def flatMap[K2 : Ordering, V2](f: ((K, V)) => IterableOnce[(K2, V2)]): CC[K2, V2] =
      sortedMapFactory.sortedFromIterable(View.FlatMap(filtered, f))

    override def withFilter(q: ((K, V)) => Boolean): SortedMapWithFilter = new SortedMapWithFilter(kv => p(kv) && q(kv))

  }

  // And finally, we add new overloads taking an ordering
  def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFromIterable(View.Map[(K, V), (K2, V2)](toIterable, f))

  def flatMap[K2, V2](f: ((K, V)) => IterableOnce[(K2, V2)])(implicit ordering: Ordering[K2]): CC[K2, V2] =
    sortedMapFromIterable(View.FlatMap(toIterable, f))

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
