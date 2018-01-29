package strawman.collection

import scala.{Any, Array, Char, Ordering, `inline`, deprecated}
import scala.Predef.String
import strawman.collection.mutable.Builder
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
  def fromSpecificIterable(from: From)(it: Iterable[A]): C

  /** Get a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
    * Building collections with `fromSpecificIterable` is preferred because it can be lazy for lazy collections. */
  def newBuilder(from: From): Builder[A, C]

  @deprecated("Use newBuilder() instead of apply()", "2.13.0")
  @`inline` def apply(from: From): Builder[A, C] = newBuilder(from)
}

object BuildFrom extends BuildFromLowPriority {
  /** Build the source collection type from a MapOps */
  implicit def buildFromMapOps[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K0, V0, K, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = from.mapFactory.newBuilder[K, V]()
    def fromSpecificIterable(from: CC[K0, V0])(it: Iterable[(K, V)]): CC[K, V] = from.mapFactory.from(it)
  }

  /** Build the source collection type from a SortedMapOps */
  implicit def buildFromSortedMapOps[CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], K0, V0, K : Ordering, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = from.sortedMapFactory.newBuilder[K, V]()
    def fromSpecificIterable(from: CC[K0, V0])(it: Iterable[(K, V)]): CC[K, V] = from.sortedMapFactory.from(it)
  }

  /** Build the source collection type from an Iterable with SortedOps */
  implicit def buildFromSortedSetOps[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, _], A0, A : Ordering]: BuildFrom[CC[A0], A, CC[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = from.sortedIterableFactory.newBuilder[A]()
    def fromSpecificIterable(from: CC[A0])(it: Iterable[A]): CC[A] = from.sortedIterableFactory.from(it)
  }

  implicit val buildFromString: BuildFrom[String, Char, String] =
    new BuildFrom[String, Char, String] {
      def fromSpecificIterable(from: String)(it: Iterable[Char]): String = Factory.stringFactory.fromSpecific(it)
      def newBuilder(from: String): Builder[Char, String] = Factory.stringFactory.newBuilder()
    }

  implicit def buildFromArray[A : ClassTag]: BuildFrom[Array[_], A, Array[A]] =
    new BuildFrom[Array[_], A, Array[A]] {
      def fromSpecificIterable(from: Array[_])(it: Iterable[A]): Array[A] = Factory.arrayFactory[A].fromSpecific(it)
      def newBuilder(from: Array[_]): Builder[A, Array[A]] = Factory.arrayFactory[A].newBuilder()
    }

}

trait BuildFromLowPriority {
  /** Build the source collection type from an IterableOps */
  implicit def buildFromIterableOps[CC[X] <: Iterable[X] with IterableOps[X, CC, _], A0, A]: BuildFrom[CC[A0], A, CC[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = from.iterableFactory.newBuilder[A]()
    def fromSpecificIterable(from: CC[A0])(it: Iterable[A]): CC[A] = from.iterableFactory.from(it)
  }
}
