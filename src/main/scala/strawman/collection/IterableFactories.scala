package strawman
package collection

import strawman.collection.mutable.Builder

import scala.{Any, Int}
import scala.annotation.unchecked.uncheckedVariance

/** Instances of Build[B] can build some kind of collection from values of type B. */
trait Build[-B] {
  type To[_]
  def fromIterable[E <: B](it: Iterable[E]): To[E]
}

trait BuildConstrained {
  trait Constraint[E] extends Build[E]
}

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[+C[_]] extends Build[Any] {
  type To[X] = C[X] @uncheckedVariance
  def fromIterable[E](it: Iterable[E]): C[E]
}

/** Base trait for instances that can construct a collection from an iterable by using an implicit evidence
  * for the element type. */
trait ConstrainedFromIterable[+CC[_], Ev[_]] {
  def constrainedFromIterable[E : Ev](it: Iterable[E]): CC[E]
}

/** Base trait for companion objects of unconstrained collection types */
trait IterableFactory[+C[_]] extends FromIterable[C] { self =>
  def empty[A]: C[A] = fromIterable(View.Empty)

  def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): C[A] = fromIterable(View.Fill(n)(elem))

  def newBuilder[A]: Builder[A, C[A]]

  protected[this] lazy val canBuildProto: CanBuild[Any, C[Any]] = new CanBuild[Any, C[Any]] {
    def fromIterable(it: Iterable[Any]): C[Any] = self.fromIterable[Any](it)
    def newBuilder: Builder[Any, C[Any]] = self.newBuilder[Any]
  }

  implicit def canBuild[E]: CanBuild[E, C[E]] = canBuildProto.asInstanceOf[CanBuild[E, C[E]]]
}

trait CanBuild[E, +Repr] {
  def fromIterable(it: Iterable[E]): Repr
  def newBuilder: Builder[E, Repr]
}

/** Base trait for companion objects of collections that require an implicit evidence */
trait ConstrainedIterableFactory[+CC[X], Ev[_]] extends ConstrainedFromIterable[CC, Ev] {

  def empty[A : Ev]: CC[A] = constrainedFromIterable(View.Empty)

  def apply[A : Ev](xs: A*): CC[A] = constrainedFromIterable(View.Elems(xs: _*))

  def fill[A : Ev](n: Int)(elem: => A): CC[A] = constrainedFromIterable(View.Fill(n)(elem))

  def constrainedNewBuilder[A : Ev]: Builder[A, CC[A]]

  implicit def canBuild[E : Ev]: CanBuild[E, CC[E]] = new CanBuild[E, CC[E]] {
    def fromIterable(it: Iterable[E]): CC[E] = constrainedFromIterable[E](it)
    def newBuilder: Builder[E, CC[E]] = constrainedNewBuilder[E]
  }
}
