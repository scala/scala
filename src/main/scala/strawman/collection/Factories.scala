package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Any, Int, Nothing, Ordering}
import scala.annotation.unchecked.uncheckedVariance

/**
  * Builds a collection of type `C` from elements of type `A`
  * @tparam A Type of elements (e.g. `Int`, `Boolean`, etc.)
  * @tparam C Type of collection (e.g. `List[Int]`, `TreeMap[Int, String]`, etc.)
  */
trait FromSpecificIterable[-A, +C] extends Any {
  def fromSpecificIterable(it: Iterable[A]): C
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+CC[_]] {

  def fromIterable[E](it: Iterable[E]): CC[E]

  def empty[A]: CC[A]

  def apply[A](xs: A*): CC[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): CC[A] = fromIterable(View.Fill(n)(elem))

}

object IterableFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[A, CC[_]](factory: IterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.fromIterable[A](it)
    }

  class Delegate[CC[_]](delegate: IterableFactory[CC]) extends IterableFactory[CC] {
    def empty[A]: CC[A] = delegate.empty
    def fromIterable[E](it: Iterable[E]): CC[E] = delegate.fromIterable(it)
  }

}

trait IterableFactoryWithBuilder[+CC[_]] extends IterableFactory[CC] {
  def newBuilder[A](): Builder[A, CC[A]]
}

object IterableFactoryWithBuilder {
  class Delegate[CC[_]](delegate: IterableFactoryWithBuilder[CC])
    extends IterableFactory.Delegate[CC](delegate)
      with IterableFactoryWithBuilder[CC] {
    def newBuilder[A](): Builder[A, CC[A]] = delegate.newBuilder()
  }
}

trait SpecificIterableFactory[-A, +C] extends FromSpecificIterable[A, C] {
  def empty: C

  def apply(xs: A*): C = fromSpecificIterable(View.Elems(xs: _*))

  def fill(n: Int)(elem: => A): C = fromSpecificIterable(View.Fill(n)(elem))
}

trait SpecificIterableFactoryWithBuilder[-A, +C] extends SpecificIterableFactory[A, C] {
  def newBuilder(): Builder[A, C]
}

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactory[+CC[_, _]] {

  def empty[K, V]: CC[K, V]
  def fromIterable[K, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K, V](elems: (K, V)*): CC[K, V] = fromIterable(elems.toStrawman)
}

object MapFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[K, V, CC[_, _]](factory: MapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.fromIterable[K, V](it)
    }

  class Delegate[CC[_, _]](delegate: MapFactory[CC]) extends MapFactory[CC] {
    def fromIterable[K, V](it: Iterable[(K, V)]): CC[K, V] = delegate.fromIterable(it)
    def empty[K, V]: CC[K, V] = delegate.empty
  }

}

trait MapFactoryWithBuilder[+CC[_, _]] extends MapFactory[CC] {
  def newBuilder[K, V](): Builder[(K, V), CC[K, V]]
}

object MapFactoryWithBuilder {

  class Delegate[CC[_, _]](delegate: MapFactoryWithBuilder[CC])
    extends MapFactory.Delegate[CC](delegate)
      with MapFactoryWithBuilder[CC] {
    def newBuilder[K, V](): Builder[(K, V), CC[K, V]] = delegate.newBuilder()
  }

}

/** Base trait for companion objects of collections that require an implicit evidence */
trait SortedIterableFactory[+CC[_]] {

  def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E]

  def empty[A : Ordering]: CC[A]

  def apply[A : Ordering](xs: A*): CC[A] = sortedFromIterable(View.Elems(xs: _*))

  def fill[A : Ordering](n: Int)(elem: => A): CC[A] = sortedFromIterable(View.Fill(n)(elem))
}

object SortedIterableFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[A: Ordering, CC[_]](factory: SortedIterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.sortedFromIterable[A](it)
    }

  class Delegate[CC[_]](delegate: SortedIterableFactory[CC]) extends SortedIterableFactory[CC] {
    def empty[A : Ordering]: CC[A] = delegate.empty
    def sortedFromIterable[E : Ordering](it: Iterable[E]): CC[E] = delegate.sortedFromIterable(it)
  }

}

trait SortedIterableFactoryWithBuilder[+CC[_]] extends SortedIterableFactory[CC] {
  def newBuilder[A : Ordering](): Builder[A, CC[A]]
}

object SortedIterableFactoryWithBuilder {
  class Delegate[CC[_]](delegate: SortedIterableFactoryWithBuilder[CC])
    extends SortedIterableFactory.Delegate[CC](delegate)
      with SortedIterableFactoryWithBuilder[CC] {
    def newBuilder[A: Ordering](): Builder[A, CC[A]] = delegate.newBuilder()
  }
}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait SortedMapFactory[+CC[X, Y]] {

  def empty[K : Ordering, V]: CC[K, V]

  def sortedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] =
    sortedFromIterable(elems.toStrawman)
}

object SortedMapFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[K : Ordering, V, CC[_, _]](factory: SortedMapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.sortedFromIterable(it)
    }

  class Delegate[CC[_, _]](delegate: SortedMapFactory[CC]) extends SortedMapFactory[CC] {
    def empty[K: Ordering, V]: CC[K, V] = delegate.empty[K, V]
    def sortedFromIterable[K: Ordering, V](it: Iterable[(K, V)]): CC[K, V] = delegate.sortedFromIterable(it)
  }

}

trait SortedMapFactoryWithBuilder[+CC[_, _]] extends SortedMapFactory[CC] {
  def newBuilder[K : Ordering, V](): Builder[(K, V), CC[K, V]]
}

object SortedMapFactoryWithBuilder {
  class Delegate[CC[_, _]](delegate: SortedMapFactoryWithBuilder[CC])
    extends SortedMapFactory.Delegate[CC](delegate)
      with SortedMapFactoryWithBuilder[CC] {
    def newBuilder[K: Ordering, V](): Builder[(K, V), CC[K, V]] = delegate.newBuilder()
  }
}
