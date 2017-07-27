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

/**
  * Builds a collection of type `C` from elements of type `A`
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait CanBuild[-A, +C] extends Any with BuildFrom[Any, A, C] {
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
  implicit def canBuildIterable[A]: CanBuild[A, CC[A]] = IterableFactory.toCanBuild(this)
}

object IterableFactory {
  implicit def toCanBuild[A, CC[_]](factory: IterableFactory[CC]): CanBuild[A, CC[A]] =
    new CanBuild[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.fromIterable[A](it)
      def newBuilder(): Builder[A, CC[A]] = factory.newBuilder[A]()
    }

  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def fromIterable[E](it: Iterable[E]): CC[E] = delegate.fromIterable(it)
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder[A]()
  }
}

trait SpecificIterableFactory[-A, +C] extends CanBuild[A, C] {
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
  implicit def canBuildMap[K, V]: CanBuild[(K, V), CC[K, V]] = MapFactory.toCanBuild(this)
}

object MapFactory {
  implicit def toCanBuild[K, V, CC[_, _]](factory: MapFactory[CC]): CanBuild[(K, V), CC[K, V]] =
    new CanBuild[(K, V), CC[K, V]] {
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
  implicit def canBuildSortedIterable[A : Ordering]: CanBuild[A, CC[A]] = SortedIterableFactory.toCanBuild(this)
}

object SortedIterableFactory {
  implicit def toCanBuild[A: Ordering, CC[_]](factory: SortedIterableFactory[CC]): CanBuild[A, CC[A]] =
    new CanBuild[A, CC[A]] {
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
  implicit def canBuildSortedMap[K : Ordering, V]: CanBuild[(K, V), CC[K, V]] = SortedMapFactory.toCanBuild(this)
}

object SortedMapFactory {
  implicit def toCanBuild[K : Ordering, V, CC[X, Y]](factory: SortedMapFactory[CC]): CanBuild[(K, V), CC[K, V]] =
    new CanBuild[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.sortedFromIterable[K, V](it)
      def newBuilder(): Builder[(K, V), CC[K, V]] = factory.newBuilder[K, V]()
    }

  class Delegate[C[X, Y]](delegate: SortedMapFactory[C]) extends SortedMapFactory[C] {
    def sortedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): C[K, V] = delegate.sortedFromIterable(it)
    def empty[K : Ordering, V]: C[K, V] = delegate.empty
    def newBuilder[K : Ordering, V](): Builder[(K, V), C[K, V]] = delegate.newBuilder()
  }
}
