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

package scala.collection

import scala.annotation.implicitNotFound
import scala.collection.mutable.Builder
import scala.collection.immutable.WrappedString
import scala.reflect.ClassTag

/** Builds a collection of type `C` from elements of type `A` when a source collection of type `From` is available.
  * Implicit instances of `BuildFrom` are available for all collection types.
  *
  * @tparam From Type of source collection
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
@implicitNotFound(msg = "Cannot construct a collection of type ${C} with elements of type ${A} based on a collection of type ${From}.")
trait BuildFrom[-From, -A, +C] extends Any { self =>
  def fromSpecific(from: From)(it: IterableOnce[A]): C

  /** Get a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
    * Building collections with `fromSpecific` is preferred because it can be lazy for lazy collections. */
  def newBuilder(from: From): Builder[A, C]

  @deprecated("Use newBuilder() instead of apply()", "2.13.0")
  @`inline` def apply(from: From): Builder[A, C] = newBuilder(from)

  /** Partially apply a BuildFrom to a Factory */
  def toFactory(from: From): Factory[A, C] = new Factory[A, C] {
    def fromSpecific(it: IterableOnce[A]): C = self.fromSpecific(from)(it)
    def newBuilder: Builder[A, C] = self.newBuilder(from)
  }
}

object BuildFrom extends BuildFromLowPriority1 {

  /** Build the source collection type from a MapOps */
  implicit def buildFromMapOps[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K0, V0, K, V]: BuildFrom[CC[K0, V0] with Map[K0, V0], (K, V), CC[K, V] with Map[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = (from: MapOps[K0, V0, CC, _]).mapFactory.newBuilder[K, V]
    def fromSpecific(from: CC[K0, V0])(it: IterableOnce[(K, V)]): CC[K, V] = (from: MapOps[K0, V0, CC, _]).mapFactory.from(it)
  }

  /** Build the source collection type from a SortedMapOps */
  implicit def buildFromSortedMapOps[CC[X, Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], K0, V0, K : Ordering, V]: BuildFrom[CC[K0, V0] with SortedMap[K0, V0], (K, V), CC[K, V] with SortedMap[K, V]] = new BuildFrom[CC[K0, V0], (K, V), CC[K, V]] {
    def newBuilder(from: CC[K0, V0]): Builder[(K, V), CC[K, V]] = (from: SortedMapOps[K0, V0, CC, _]).sortedMapFactory.newBuilder[K, V]
    def fromSpecific(from: CC[K0, V0])(it: IterableOnce[(K, V)]): CC[K, V] = (from: SortedMapOps[K0, V0, CC, _]).sortedMapFactory.from(it)
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

  implicit val buildFromWrappedString: BuildFrom[WrappedString, Char, WrappedString] =
    new BuildFrom[WrappedString, Char, WrappedString] {
      def fromSpecific(from: WrappedString)(it: IterableOnce[Char]): WrappedString = WrappedString.fromSpecific(it)
      def newBuilder(from: WrappedString): mutable.Builder[Char, WrappedString] = WrappedString.newBuilder
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
  // Restating the upper bound of CC in the result type seems redundant, but it serves to prune the
  // implicit search space for faster compilation and reduced change of divergence. See the compilation
  // test in test/junit/scala/collection/BuildFromTest.scala and discussion in https://github.com/scala/scala/pull/10209
  implicit def buildFromSortedSetOps[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, _], A0, A : Ordering]: BuildFrom[CC[A0] with SortedSet[A0], A, CC[A] with SortedSet[A]] = new BuildFrom[CC[A0], A, CC[A]] {
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = (from: SortedSetOps[A0, CC, _]).sortedIterableFactory.newBuilder[A]
    def fromSpecific(from: CC[A0])(it: IterableOnce[A]): CC[A] = (from: SortedSetOps[A0, CC, _]).sortedIterableFactory.from(it)
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
    def newBuilder(from: CC[A0]): Builder[A, CC[A]] = (from: IterableOps[A0, CC, _]).iterableFactory.newBuilder[A]
    def fromSpecific(from: CC[A0])(it: IterableOnce[A]): CC[A] = (from: IterableOps[A0, CC, _]).iterableFactory.from(it)
  }

  implicit def buildFromIterator[A]: BuildFrom[Iterator[_], A, Iterator[A]] = new BuildFrom[Iterator[_], A, Iterator[A]] {
    def newBuilder(from: Iterator[_]): mutable.Builder[A, Iterator[A]] = Iterator.newBuilder
    def fromSpecific(from: Iterator[_])(it: IterableOnce[A]): Iterator[A] = Iterator.from(it)
  }
}
