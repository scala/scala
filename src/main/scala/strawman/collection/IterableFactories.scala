package strawman
package collection

import strawman.collection.mutable.Builder

import scala.Int

/** Base trait for instances that can construct a collection from an iterable */
trait FromIterable[+C[X] <: Iterable[X]] {
  def fromIterable[B](it: Iterable[B]): C[B]
}

/** Base trait for companion objects of collections */
trait IterableFactories[+C[X] <: Iterable[X]] extends FromIterable[C] {

  def empty[A]: C[A] = fromIterable(View.Empty)

  def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs: _*))

  def fill[A](n: Int)(elem: => A): C[A] = fromIterable(View.Fill(n)(elem))

  def newBuilder[A]: Builder[A, C[A]]

  implicit def canBuild[A]: () => Builder[A, C[A]] = () => newBuilder[A] // TODO Reuse the same instance

}
