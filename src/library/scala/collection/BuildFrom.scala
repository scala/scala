package scala.collection

import scala.language.higherKinds

import scala.collection.mutable.Builder
import scala.annotation.implicitNotFound

import scala.reflect.ClassTag

/** Builds a collection of type `C` from elements of type `A` when a source collection of type `From` is available.
  * Implicit instances of `BuildFrom` are available for all collection types.
  *
  * @tparam From Type of source collection
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
@implicitNotFound(msg = "Cannot construct a collection of type ${C} with elements of type ${A} based on a collection of type ${From}.")
trait BuildFrom[-From, -A, +C] extends Any {
  def fromSpecific(from: From)(it: IterableOnce[A]): C

  /** Get a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
    * Building collections with `fromSpecific` is preferred because it can be lazy for lazy collections. */
  def newBuilder(from: From): Builder[A, C]

  @deprecated("Use newBuilder() instead of apply()", "2.13.0")
  @`inline` def apply(from: From): Builder[A, C] = newBuilder(from)
}

object BuildFrom extends BuildFromLowPriority1 {

  /** Build the source collection type from a MapOps */
  implicit def buildFromMapOps[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K0, V0, K, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = from.mapFactory.newBuilder[K, V]
    def fromSpecific(from: CC[K0, V0])(it: IterableOnce[(K, V)]): CC[K, V] = from.mapFactory.from(it)
  }

  /** Build the source collection type from a SortedMapOps */
  implicit def buildFromSortedMapOps[CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], K0, V0, K : Ordering, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = from.sortedMapFactory.newBuilder[K, V]
    def fromSpecific(from: CC[K0, V0])(it: IterableOnce[(K, V)]): CC[K, V] = from.sortedMapFactory.from(it)
  }

  implicit def buildFromBitSet[C <: BitSet with BitSetOps[C]]: BuildFrom[C, Int, C] =
    new BuildFrom[C, Int, C] {
      def fromSpecific(from: C)(it: IterableOnce[Int]): C = from.bitSetFactory.fromSpecific(it)
      def newBuilder(from: C): Builder[Int, C] = from.bitSetFactory.newBuilder
    }

  implicit val buildFromString: BuildFrom[String, Char, String] =
    new BuildFrom[String, Char, String] {
      def fromSpecific(from: String)(it: IterableOnce[Char]): String = Factory.stringFactory.fromSpecific(it)
      def newBuilder(from: String): Builder[Char, String] = Factory.stringFactory.newBuilder
    }

  implicit def buildFromArray[A : ClassTag]: BuildFrom[Array[_], A, Array[A]] =
    new BuildFrom[Array[_], A, Array[A]] {
      def fromSpecific(from: Array[_])(it: IterableOnce[A]): Array[A] = Factory.arrayFactory[A].fromSpecific(it)
      def newBuilder(from: Array[_]): Builder[A, Array[A]] = Factory.arrayFactory[A].newBuilder
    }

  implicit def buildFromView[A, B]: BuildFrom[View[A], B, View[B]] =
    new BuildFrom[View[A], B, View[B]] {
      def fromSpecific(from: View[A])(it: IterableOnce[B]): View[B] = View.from(it)
      def newBuilder(from: View[A]): Builder[B, View[B]] = View.newBuilder
    }

}

trait BuildFromLowPriority1 extends BuildFromLowPriority2 {

  /** Build the source collection type from an Iterable with SortedOps */
  implicit def buildFromSortedSetOps[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, _], A0, A : Ordering]: BuildFrom[CC[A0], A, CC[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = from.sortedIterableFactory.newBuilder[A]
    def fromSpecific(from: CC[A0])(it: IterableOnce[A]): CC[A] = from.sortedIterableFactory.from(it)
  }

  implicit def fallbackStringCanBuildFrom[A]: BuildFrom[String, A, immutable.IndexedSeq[A]] =
    new BuildFrom[String, A, immutable.IndexedSeq[A]] {
      def fromSpecific(from: String)(it: IterableOnce[A]): immutable.IndexedSeq[A] = immutable.IndexedSeq.from(it)
      def newBuilder(from: String): Builder[A, immutable.IndexedSeq[A]] = immutable.IndexedSeq.newBuilder[A]
  }
}

trait BuildFromLowPriority2 {
  /** Build the source collection type from an IterableOps */
  implicit def buildFromIterableOps[CC[X] <: Iterable[X] with IterableOps[X, CC, _], A0, A]: BuildFrom[CC[A0], A, CC[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = from.iterableFactory.newBuilder[A]
    def fromSpecific(from: CC[A0])(it: IterableOnce[A]): CC[A] = from.iterableFactory.from(it)
  }

  implicit def buildFromIterator[A]: BuildFrom[Iterator[_], A, Iterator[A]] = new BuildFrom[Iterator[_], A, Iterator[A]] {
    def newBuilder(from: Iterator[_]): mutable.Builder[A, Iterator[A]] = Iterator.newBuilder
    def fromSpecific(from: Iterator[_])(it: IterableOnce[A]): Iterator[A] = Iterator.from(it)
  }
}
