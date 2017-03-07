package strawman
package collection

import strawman.collection.mutable.Builder

import scala.Int
import scala.annotation.unchecked.uncheckedVariance

trait FromIterableBase[-B] {
  type To[X]
  def fromIterable[E <: B](it: Iterable[E]): To[E]
}

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[-B, +C[_]] extends FromIterableBase[B] {
  type To[X] = C[X] @uncheckedVariance
  def fromIterable[E <: B](it: Iterable[E]): C[E]
}

/** Base trait for instances that can construct a collection from an iterable by using an implicit evidence
  * for the element type. */
trait ConstrainedFromIterable[+CC[_], Ev[_]] {
  def constrainedFromIterable[E : Ev](it: Iterable[E]): CC[E]
}

/** Base trait for companion objects of collections */
trait IterableFactory[-B, +C[_]] extends FromIterable[B, C] { self =>

  def empty[A <: B]: C[A] = fromIterable(View.Empty)

  def apply[A <: B](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))

  def fill[A <: B](n: Int)(elem: => A): C[A] = fromIterable(View.Fill(n)(elem))

  def newBuilder[A <: B]: Builder[A, C[A]]

  protected[this] lazy val monomorphicIterableFactoryProto: MonomorphicIterableFactory[B, C[B]] = new MonomorphicIterableFactory[B, C[B]] {
    def fromIterable[E <: B](it: Iterable[E]): C[B] = self.fromIterable[B](it)
    def newBuilder[A <: B]: Builder[A, C[B]] = self.newBuilder[B]
    override protected[this] lazy val monomorphicIterableFactoryProto = this
  }

  implicit def iterableFactory[E <: B]: MonomorphicIterableFactory[E, C[E]] =
    monomorphicIterableFactoryProto.asInstanceOf[MonomorphicIterableFactory[E, C[E]]]
}

trait MonomorphicIterableFactory[-B, +Repr] extends IterableFactory[B, ({ type L[X] = Repr })#L]

/** Base trait for companion objects of collections that require an implicit evidence */
trait ConstrainedIterableFactory[+CC[X], Ev[_]] extends ConstrainedFromIterable[CC, Ev] {

  def empty[A : Ev]: CC[A] = constrainedFromIterable(View.Empty)

  def apply[A : Ev](xs: A*): CC[A] = constrainedFromIterable(View.Elems(xs: _*))

  def fill[A : Ev](n: Int)(elem: => A): CC[A] = constrainedFromIterable(View.Fill(n)(elem))

  def constrainedNewBuilder[A : Ev]: Builder[A, CC[A]]

  implicit def iterableFactory[B : Ev]: MonomorphicIterableFactory[B, CC[B]] = new MonomorphicIterableFactory[B, CC[B]] {
    def fromIterable[E <: B](it: Iterable[E]): CC[B] = constrainedFromIterable[B](it)
    def newBuilder[A <: B]: Builder[A, CC[B]] = constrainedNewBuilder[B]
  }
}
