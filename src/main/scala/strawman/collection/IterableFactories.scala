package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Any, Int, Ordering, Nothing}
import scala.annotation.unchecked.uncheckedVariance

/** Base trait for instances that can construct an unconstrained collection from an iterable */
trait FromIterable[+C[_]] extends Any {
  def fromIterable[E](it: Iterable[E]): C[E]
}

/** Base trait for instances that can construct a collection from an iterable by using an implicit evidence
  * for the element type. */
trait OrderedFromIterable[+CC[_]] extends Any {
  def orderedFromIterable[E : Ordering](it: Iterable[E]): CC[E]
}

/** Base trait for companion objects of collection types that may require an upper bound but no implicit evidence */
trait BoundedIterableFactory[-B] { self =>
  /** The result type to build for an element type. This is a type member instead of a type parameter because
    * it doesn't have to be a proper type constructor (e.g. in `BitSet` we have `type To[_] = BitSet`). */
  type To[_]

  def fromIterable[E <: B](it: Iterable[E]): To[E]

  def empty[A <: B]: To[A]

  def apply[A <: B](xs: A*): To[A] = fromIterable(View.Elems(xs: _*))

  def fill[A <: B](n: Int)(elem: => A): To[A] = fromIterable(View.Fill(n)(elem))

  def newBuilder[A <: B]: Builder[A, To[A]]
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+C[_]] extends BoundedIterableFactory[Any] with FromIterable[C] {
  type To[X] = C[X] @uncheckedVariance
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait OrderedIterableFactory[+CC[X]] extends OrderedFromIterable[CC] {

  def empty[A : Ordering]: CC[A]

  def apply[A : Ordering](xs: A*): CC[A] = orderedFromIterable(View.Elems(xs: _*))

  def fill[A : Ordering](n: Int)(elem: => A): CC[A] = orderedFromIterable(View.Fill(n)(elem))

  def orderedNewBuilder[A : Ordering]: Builder[A, CC[A]]
}
