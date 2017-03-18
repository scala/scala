package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Any, Int, Nothing}
import scala.annotation.unchecked.uncheckedVariance

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[+C[_]] extends Any with TypeConstrainedFromIterable[Any] {
  type To[X] = C[X] @uncheckedVariance
}

/** Base trait for instances that can construct a collection from an iterable for certain types
  * (but without needing an evidence value). */
trait TypeConstrainedFromIterable[-B] extends Any {
  type To[_]
  def fromIterable[E <: B](it: Iterable[E]): To[E]
}

/** Base trait for instances that can construct a collection from an iterable by using an implicit evidence
  * for the element type. */
trait ConstrainedFromIterable[+CC[_], Ev[_]] extends Any {
  def constrainedFromIterable[E : Ev](it: Iterable[E]): CC[E]
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+C[_]] extends FromIterable[C] { self =>
  def empty[A]: C[A] = fromIterable(View.Empty)

  def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): C[A] = fromIterable(View.Fill(n)(elem))

  def newBuilder[A]: Builder[A, C[A]]
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait ConstrainedIterableFactory[+CC[X], Ev[_]] extends ConstrainedFromIterable[CC, Ev] {

  def empty[A : Ev]: CC[A] = constrainedFromIterable(View.Empty)

  def apply[A : Ev](xs: A*): CC[A] = constrainedFromIterable(View.Elems(xs: _*))

  def fill[A : Ev](n: Int)(elem: => A): CC[A] = constrainedFromIterable(View.Fill(n)(elem))

  def constrainedNewBuilder[A : Ev]: Builder[A, CC[A]]
}
