package strawman
package collection

import scala.language.implicitConversions

import strawman.collection.mutable.Builder

import scala.{Any, Int, Nothing, Ordering}
import scala.annotation.unchecked.uncheckedVariance


/** Builds a collection of type `C` from elements of type `A` when a source collection of type `From` is available.
  * Implicit instances of `BuildFrom` are available for all collection types.
  *
  * @tparam From Type of source collection
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait BuildFrom[-From, -A, +C] extends Any {
  def fromSpecificIterable(from: From)(it: Iterable[A]): C

  /** Get a Builder for the collection. For non-strict collection types this will use an intermediate buffer.
    * Building collections with `fromSpecificIterable` is preferred because it can be lazy for lazy collections. */
  def newBuilder(from: From): Builder[A, C]
}

object BuildFrom extends BuildFromLowPriority {
  /** Build the source collection type from a MapOps */
  implicit def buildFromMapOps[CC[K, V] <: Map[K, V] with MapOps[K, V, CC, _], A, B, E, F]: BuildFrom[CC[A, B], (E, F), CC[E, F]] = new BuildFrom[CC[A, B], (E, F), CC[E, F]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A, B]): Builder[(E, F), CC[E, F]] = from.mapFactory.newBuilder[E, F]()
    def fromSpecificIterable(from: CC[A, B])(it: Iterable[(E, F)]): CC[E, F] = from.mapFactory.fromIterable(it)
  }

  /** Build the source collection type from a SortedMapOps */
  implicit def buildFromSortedMapOps[CC[K, V] <: SortedMap[K, V] with SortedMapOps[K, V, CC, _], A, B, E : Ordering, F]: BuildFrom[CC[A, B], (E, F), CC[E, F]] = new BuildFrom[CC[A, B], (E, F), CC[E, F]] {
    def newBuilder(from: CC[A, B]): Builder[(E, F), CC[E, F]] = from.sortedMapFactory.newBuilder[E, F]()
    def fromSpecificIterable(from: CC[A, B])(it: Iterable[(E, F)]): CC[E, F] = from.sortedMapFactory.fromSpecificIterable(it)
  }
}

trait BuildFromLowPriority {
  /** Build the source collection type from an IterableOps */
  implicit def buildFromIterableOps[CC[X] <: Iterable[X] with IterableOps[X, CC, _], A, E]: BuildFrom[CC[A], E, CC[E]] = new BuildFrom[CC[A], E, CC[E]] {
    //TODO: Reuse a prototype instance
    def newBuilder(from: CC[A]): Builder[E, CC[E]] = from.iterableFactory.newBuilder[E]()
    def fromSpecificIterable(from: CC[A])(it: Iterable[E]): CC[E] = from.iterableFactory.fromIterable(it)
  }

  /** Build the source collection type from an Iterable with SortedOps */
  implicit def buildFromSortedOps[CC[X] <: Iterable[X] with SortedOps[X, CC, _], A, E : Ordering]: BuildFrom[CC[A], E, CC[E]] = new BuildFrom[CC[A], E, CC[E]] {
    def newBuilder(from: CC[A]): Builder[E, CC[E]] = from.sortedIterableFactory.newBuilder[E]()
    def fromSpecificIterable(from: CC[A])(it: Iterable[E]): CC[E] = from.sortedIterableFactory.fromSpecificIterable(it)
  }
}

/**
  * Builds a collection of type `C` from elements of type `A`
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait FromSpecificIterable[-A, +C] extends Any with BuildFrom[Any, A, C] {
  def fromSpecificIterable(from: Any)(it: Iterable[A]): C = fromSpecificIterable(it)
  def fromSpecificIterable(it: Iterable[A]): C
  def newBuilder(from: Any): Builder[A, C] = newBuilder()
  def newBuilder(): Builder[A, C]
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+CC[_]] {
  def fromIterable[E](it: Iterable[E]): CC[E]
  def empty[A]: CC[A]
  def apply[A](xs: A*): CC[A] = fromIterable(View.Elems(xs: _*))
  def fill[A](n: Int)(elem: => A): CC[A] = fromIterable(View.Fill(n)(elem))
  def newBuilder[A](): Builder[A, CC[A]]
}

object IterableFactory {
  implicit def toSpecific[A, CC[_]](factory: IterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.fromIterable[A](it)
      def newBuilder(): Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def fromIterable[E](it: Iterable[E]): CC[E] = delegate.fromIterable(it)
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

trait SpecificIterableFactory[-A, +C] extends FromSpecificIterable[A, C] {
  def empty: C
  def apply(xs: A*): C = fromSpecificIterable(View.Elems(xs: _*))
  def fill(n: Int)(elem: => A): C = fromSpecificIterable(View.Fill(n)(elem))
  def newBuilder(): Builder[A, C]
}

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactory[+CC[X, Y]] {
  def empty[K, V]: CC[K, V]
  def fromIterable[K, V](it: Iterable[(K, V)]): CC[K, V]
  def apply[K, V](elems: (K, V)*): CC[K, V] = fromIterable(elems.toStrawman)
  def newBuilder[K, V](): Builder[(K, V), CC[K, V]]
}

object MapFactory {
  implicit def toSpecific[K, V, CC[_, _]](factory: MapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.fromIterable[K, V](it)
      def newBuilder(): Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  class Delegate[C[X, Y]](delegate: MapFactory[C]) extends MapFactory[C] {
    def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] = delegate.fromIterable(it)
    def empty[K, V]: C[K, V] = delegate.empty
    def newBuilder[K, V](): Builder[(K, V), C[K, V]] = delegate.newBuilder()
  }
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait SortedIterableFactory[+CC[_]] {
  def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E]
  def empty[A : Ordering]: CC[A]
  def apply[A : Ordering](xs: A*): CC[A] = sortedFromIterable(View.Elems(xs: _*))
  def fill[A : Ordering](n: Int)(elem: => A): CC[A] = sortedFromIterable(View.Fill(n)(elem))
  def newBuilder[A : Ordering](): Builder[A, CC[A]]
}

object SortedIterableFactory {
  implicit def toSpecific[A: Ordering, CC[_]](factory: SortedIterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.sortedFromIterable[A](it)
      def newBuilder(): Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  class Delegate[CC[_]](delegate: SortedIterableFactory[CC]) extends SortedIterableFactory[CC] {
    def empty[A : Ordering]: CC[A] = delegate.empty
    def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E] = delegate.sortedFromIterable(it)
    def newBuilder[A : Ordering](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait SortedMapFactory[+CC[X, Y]] {
  def empty[K : Ordering, V]: CC[K, V]
  def sortedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): CC[K, V]
  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] =
    sortedFromIterable(elems.toStrawman)
  def newBuilder[K : Ordering, V](): Builder[(K, V), CC[K, V]]
}

object SortedMapFactory {
  implicit def toSpecific[K : Ordering, V, CC[X, Y]](factory: SortedMapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.sortedFromIterable[K, V](it)
      def newBuilder(): Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  class Delegate[C[X, Y]](delegate: SortedMapFactory[C]) extends SortedMapFactory[C] {
    def sortedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): C[K, V] = delegate.sortedFromIterable(it)
    def empty[K : Ordering, V]: C[K, V] = delegate.empty
    def newBuilder[K : Ordering, V](): Builder[(K, V), C[K, V]] = delegate.newBuilder()
  }
}
