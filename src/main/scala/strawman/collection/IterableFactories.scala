package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Any, Int, Nothing}
import scala.annotation.unchecked.uncheckedVariance

/** Instances of Build[B] can build some kind of collection from values of type B. */
trait Build[-B] {
  type To[_]
  def fromIterable[E <: B](it: Iterable[E]): To[E]
}
trait BuildStrict[-B] extends Build[B] {
  def newBuilder[E <: B]: Builder[E, To[E]]
}

/** Instances of BuildConstrained can build some kind of collection from values with an implicit constraint. */
trait BuildConstrained {
  trait Constraint[E] <: Build[E]
}
trait BuildStrictConstrained extends BuildConstrained {
  trait Constraint[E] extends super.Constraint[E] with BuildStrict[E]
}

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[+C[_]] extends Build[Any] {
  type To[X] = C[X] @uncheckedVariance
  def fromIterable[E](it: Iterable[E]): C[E]
}

/** Base trait for instances that can construct a collection from an iterable by using an implicit evidence
  * for the element type. */
trait ConstrainedFromIterable[+CC[_], Ev[_]] extends BuildConstrained {
  def constrainedFromIterable[E : Ev](it: Iterable[E]): CC[E]
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+C[_]] extends FromIterable[C] with BuildStrict[Any] { self =>
  def empty[A]: C[A] = fromIterable(View.Empty)

  def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): C[A] = fromIterable(View.Fill(n)(elem))

  def newBuilder[A]: Builder[A, C[A]]

  protected[this] lazy val buildFromProto: BuildFrom[C[Any], Any] = new BuildFrom[C[Any], Any] {
    type To[_] = C[Any]
    def fromIterable[E](it: Iterable[E]): C[Any] = self.fromIterable[Any](it)
    def newBuilder[E]: Builder[E, C[Any]] = self.newBuilder[Any]
  }

  implicit def buildFrom[F, E]: BuildFrom[C[F], E] { type To[_] <: C[E] } =
    buildFromProto.asInstanceOf[BuildFrom[C[F], E] { type To[_] <: C[E] }]

  def buildFromAny[E]: BuildFrom[Nothing, E] { type To[_] <: C[E] } =
    buildFromProto.asInstanceOf[BuildFrom[Nothing, E] { type To[_] <: C[E] }]
}

/** Implicit instances of this type are available for building arbitrary collection types */
trait BuildFrom[+Repr, E] extends BuildStrict[E] { self =>
  def any: BuildFrom[Nothing, E] { type To[X] = self.To[X] } = this.asInstanceOf[BuildFrom[Nothing, E] { type To[X] = self.To[X] }]
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait ConstrainedIterableFactory[+CC[X], Ev[_]] extends ConstrainedFromIterable[CC, Ev] with BuildStrictConstrained {

  class ConstraintImpl[E : Ev] extends Constraint[E] {
    type To[_] = CC[E] @uncheckedVariance
    def fromIterable[X <: E](it: Iterable[X]): CC[E] = constrainedFromIterable[E](it)
    def newBuilder[X <: E]: Builder[X, CC[E]] = constrainedNewBuilder[E]
  }
  implicit def constraint[E : Ev]: ConstraintImpl[E] = new ConstraintImpl[E]

  def empty[A : Ev]: CC[A] = constrainedFromIterable(View.Empty)

  def apply[A : Ev](xs: A*): CC[A] = constrainedFromIterable(View.Elems(xs: _*))

  def fill[A : Ev](n: Int)(elem: => A): CC[A] = constrainedFromIterable(View.Fill(n)(elem))

  def constrainedNewBuilder[A : Ev]: Builder[A, CC[A]]

  implicit def buildFrom[F, E : Ev]: BuildFrom[CC[F], E] { type To[_] <: CC[E] } = new BuildFrom[CC[F], E] {
    type To[_] = CC[E]
    def fromIterable[X <: E](it: Iterable[X]): CC[E] = constrainedFromIterable[E](it)
    def newBuilder[X <: E]: Builder[X, CC[E]] = constrainedNewBuilder[E]
  }

  def buildFromAny[E : Ev]: BuildFrom[Nothing, E] { type To[_] <: CC[E] } =
    buildFrom[Any, E].asInstanceOf[BuildFrom[Nothing, E] { type To[_] <: CC[E] }]
}
