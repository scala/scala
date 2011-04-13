/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import mutable.{ Buffer, ListBuffer, ArrayBuffer }
import annotation.unchecked.{ uncheckedVariance => uV }

/** A template trait for collections which can be traversed either once only
 *  or one or more times.
 *
 *  All of the methods in this trait are guaranteed to be implemented
 *  in a single-threaded manner.
 *
 *  $traversableonceinfo
 *
 *  @tparam A    the element type of the collection
 *
 *  @author Martin Odersky
 *  @author Paul Phillips
 *  @version 2.8
 *  @since   2.8
 *
 *  @define coll traversable or iterator
 *  @define orderDependent
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *  @define orderDependentFold
 *
 *    Note: might return different results for different runs, unless the
 *    underlying collection type is ordered or the operator is associative
 *    and commutative.
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 */
trait TraversableOnce[+A] extends GenTraversableOnce[A] with TraversableOnceLike[A] {
  self =>

  override def seq: TraversableOnce[A] = this

}


object TraversableOnce {
  implicit def traversableOnceCanBuildFrom[T] = new OnceCanBuildFrom[T]
  implicit def wrapTraversableOnce[A](trav: TraversableOnce[A]) = new MonadOps(trav)
  implicit def flattenTraversableOnce[A, CC[_]](travs: TraversableOnce[CC[A]])(implicit ev: CC[A] => TraversableOnce[A]) =
    new FlattenOps[A](travs map ev)

  /** With the advent of TraversableOnce, it can be useful to have a builder which
   *  operates on Iterators so they can be treated uniformly along with the collections.
   *  See scala.util.Random.shuffle for an example.
   */
  class OnceCanBuildFrom[A] extends generic.CanBuildFrom[TraversableOnce[A], A, TraversableOnce[A]] {
    def newIterator = new ArrayBuffer[A] mapResult (_.iterator)

    /** Creates a new builder on request of a collection.
     *  @param from  the collection requesting the builder to be created.
     *  @return the result of invoking the `genericBuilder` method on `from`.
     */
    def apply(from: TraversableOnce[A]) = newIterator

    /** Creates a new builder from scratch
     *  @return the result of invoking the `newBuilder` method of this factory.
     */
    def apply() = newIterator
  }

  class FlattenOps[A](travs: TraversableOnce[TraversableOnce[A]]) {
    def flatten: Iterator[A] = travs.foldLeft(Iterator.empty: Iterator[A])(_ ++ _)
  }

  class MonadOps[+A](trav: TraversableOnce[A]) {
    def map[B](f: A => B): TraversableOnce[B] = trav.toIterator map f
    def flatMap[B](f: A => GenTraversableOnce[B]): TraversableOnce[B] = trav.toIterator flatMap f
    def withFilter(p: A => Boolean) = trav.toIterator filter p
    def filter(p: A => Boolean): TraversableOnce[A] = withFilter(p)
  }
}
