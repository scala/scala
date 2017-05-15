package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Any, Int, Ordering, Nothing}
import scala.annotation.unchecked.uncheckedVariance

/** Base trait for instances that can construct an unconstrained collection from an iterable */
trait FromIterable[+CC[_]] extends Any {
  def fromIterable[E](it: Iterable[E]): CC[E]
}

object FromIterable {
  implicit def toSpecific[A, CC[_]](fi: FromIterable[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = fi.fromIterable[A](it)
    }
}

trait FromSpecificIterable[-A, +C] extends Any {
  def fromSpecificIterable(it: Iterable[A]): C
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+CC[_]] extends FromIterable[CC] {
  def empty[A]: CC[A]

  def apply[A](xs: A*): CC[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): CC[A] = fromIterable(View.Fill(n)(elem))
}

object IterableFactory {

  class Delegate[CC[_]](protected val delegate: IterableFactory[CC]) extends IterableFactory[CC] {
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
  implicit def toSpecific[K, V, CC[X, Y]]
      (fi: MapFactory[CC]): FromSpecificIterable[(K, V), CC[K, V]] =
    new FromSpecificIterable[(K, V), CC[K, V]] {
      def fromSpecificIterable(it: Iterable[(K, V)]): CC[K, V] = fi.fromIterable[K, V](it)
    }

  class Delegate[C[X, Y]](delegate: MapFactory[C]) extends MapFactory[C] {
    def fromIterable[K, V](it: Iterable[(K, V)]): C[K, V] = delegate.fromIterable(it)
    def empty[K, V]: C[K, V] = delegate.empty
  }

}

trait OrderedFromIterable[+CC[_]] extends Any {
  def orderedFromIterable[E : Ordering](it: Iterable[E]): CC[E]
}

object OrderedFromIterable {
  implicit def toSpecific[A: Ordering, CC[_]](fi: OrderedFromIterable[CC]): FromSpecificIterable[A, CC[A]] =
    new FromSpecificIterable[A, CC[A]] {
      def fromSpecificIterable(it: Iterable[A]): CC[A] = fi.orderedFromIterable[A](it)
    }
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait OrderedSetFactory[+CC[_]] extends OrderedFromIterable[CC] {

  def empty[A : Ordering]: CC[A]

  def apply[A : Ordering](xs: A*): CC[A] = orderedFromIterable(View.Elems(xs: _*))

  def fill[A : Ordering](n: Int)(elem: => A): CC[A] = orderedFromIterable(View.Fill(n)(elem))
}

object OrderedSetFactory {

  class Delegate[CC[_]](protected val delegate: OrderedSetFactory[CC]) extends OrderedSetFactory[CC] {
    def empty[A : Ordering]: CC[A] = delegate.empty
    def orderedFromIterable[E : Ordering](it: Iterable[E]): CC[E] = delegate.orderedFromIterable(it)
  }

}

/** Factory methods for collections of kind `* −> * -> *` which require an implicit evidence value for the key type */
trait OrderedMapFactory[+CC[X, Y]] {

  def empty[K : Ordering, V]: CC[K, V]

  def fromIterable[K : Ordering, V](it: Iterable[(K, V)]): CC[K, V]

  def apply[K : Ordering, V](elems: (K, V)*): CC[K, V] =
    fromIterable(elems.toStrawman)
}
