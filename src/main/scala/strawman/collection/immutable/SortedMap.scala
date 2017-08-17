package strawman
package collection
package immutable

import strawman.collection.mutable.Builder

import scala.{Boolean, Int, Option, Ordering, Serializable}

trait SortedMap[K, +V]
  extends Map[K, V]
     with collection.SortedMap[K, V]
     with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]

trait SortedMapOps[K, +V, +CC[X, +Y] <: Map[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMapOps[K, V, CC, C]]
  extends MapOps[K, V, Map, C]
     with collection.SortedMapOps[K, V, CC, C] { self =>

    protected[this] def coll: C with CC[K, V]

    override def keySet: SortedSet[K] = new ImmutableKeySortedSet

    /** The implementation class of the set returned by `keySet` */
    protected class ImmutableKeySortedSet extends SortedSet[K] with GenKeySet with GenKeySortedSet {
      def iterableFactory: IterableFactory[Set] = Set
      def sortedIterableFactory: SortedIterableFactory[SortedSet] = SortedSet
      protected[this] def sortedFromIterable[B: Ordering](it: collection.Iterable[B]): SortedSet[B] = sortedIterableFactory.sortedFromIterable(it)
      protected[this] def fromSpecificIterable(coll: collection.Iterable[K]): SortedSet[K] = sortedIterableFactory.sortedFromIterable(coll)
      protected[this] def newSpecificBuilder(): Builder[K, SortedSet[K]] = sortedIterableFactory.newBuilder()
      def rangeImpl(from: Option[K], until: Option[K]): SortedSet[K] = {
        val map = self.rangeImpl(from, until)
        new map.ImmutableKeySortedSet
      }
      def empty: SortedSet[K] = sortedIterableFactory.empty
      def incl(elem: K): SortedSet[K] = fromSpecificIterable(this).incl(elem)
      def excl(elem: K): SortedSet[K] = fromSpecificIterable(this).excl(elem)
    }

    protected def mapFromIterable[K2, V2](it: collection.Iterable[(K2, V2)]): Map[K2, V2] =
      Map.fromIterable(it)

    // We override these methods to fix their return type (which would be `Map` otherwise)
    def updated[V1 >: V](key: K, value: V1): CC[K, V1]
    override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)

    override def concat[V2 >: V](xs: collection.Iterable[(K, V2)]): CC[K, V2] = {
        var result: CC[K, V2] = coll
        val it = xs.iterator()
        while (it.hasNext) result = result + it.next()
        result
    }

}

object SortedMap extends SortedMapFactory.Delegate[SortedMap](TreeMap)