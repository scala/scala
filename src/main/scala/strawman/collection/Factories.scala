package strawman
package collection

import scala.{Any, Int, Ordering, Nothing}
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

trait SpecificIterableFactory[-A, +C] extends FromSpecificIterable[A, C] {
  def empty: C

  def apply(xs: A*): C = fromSpecificIterable(View.Elems(xs: _*))

  def fill(n: Int)(elem: => A): C = fromSpecificIterable(View.Fill(n)(elem))
}

/** Factory methods for collections of kind `* −> * -> *` */
trait MapFactory[+CC[X, Y]] {

  def empty[K, V]: CC[K, V]
  def fromIterable[K, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K, V](elems: (K, V)*): CC[K, V] = fromIterable(elems.toStrawman)
}

object MapFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[K, V, CC[X, Y]](factory: MapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.fromIterable[K, V](it)
    }

  class Delegate[C[X, Y]](delegate: MapFactory[C]) extends MapFactory[C] {
    def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] = delegate.fromIterable(it)
    def empty[K, V]: C[K, V] = delegate.empty
  }

}

/** Base trait for companion objects of collections that require an implicit evidence */
trait OrderedIterableFactory[+CC[_]] {

  def orderedFromIterable[E : Ordering](it: Iterable[E]): CC[E]

  def empty[A : Ordering]: CC[A]

  def apply[A : Ordering](xs: A*): CC[A] = orderedFromIterable(View.Elems(xs: _*))

  def fill[A : Ordering](n: Int)(elem: => A): CC[A] = orderedFromIterable(View.Fill(n)(elem))
}

object OrderedIterableFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[A: Ordering, CC[_]](factory: OrderedIterableFactory[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = factory.orderedFromIterable[A](it)
    }

  class Delegate[CC[_]](delegate: OrderedIterableFactory[CC]) extends OrderedIterableFactory[CC] {
    def empty[A : Ordering]: CC[A] = delegate.empty
    def orderedFromIterable[E : Ordering](it: Iterable[E]): CC[E] = delegate.orderedFromIterable(it)
  }

}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait OrderedMapFactory[+CC[X, Y]] {

  def empty[K : Ordering, V]: CC[K, V]

  def orderedFromIterable[K : Ordering, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] =
    orderedFromIterable(elems.toStrawman)
}

object OrderedMapFactory {
  import scala.language.implicitConversions

  implicit def toSpecific[K : Ordering, V, CC[_, _]](factory: OrderedMapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = factory.orderedFromIterable(it)
    }

  class Delegate[CC[_, _]](delegate: OrderedMapFactory[CC]) extends OrderedMapFactory[CC] {
    def empty[K: Ordering, V]: CC[K, V] = delegate.empty[K, V]
    def orderedFromIterable[K: Ordering, V](it: Iterable[(K, V)]): CC[K, V] = delegate.orderedFromIterable(it)
  }

}