/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._

/** A template trait for all traversable collections upon which operations
 *  may be implemented in parallel.
 *
 *  @define thatinfo the class of the returned collection. Where possible, `That` is
 *    the same class as the current collection class `Repr`, but this
 *    depends on the element type `B` being admissible for that class,
 *    which means that an implicit instance of type `CanBuildFrom[Repr, B, That]`
 *    is found.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines
 *    the result class `That` from the current representation type `Repr` and
 *    and the new element type `B`.
 *  @define orderDependent
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *  @define orderDependentFold
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *    or the operator is associative and commutative.
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 *
 *  @define Coll GenTraversable
 *  @define coll collection
 *  @tparam T    the collection element type.
 *  @tparam Repr the actual type of the element container.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenTraversableLike[+T, +Repr] extends GenTraversableOnceLike[T] with Parallelizable[T, parallel.ParIterable[T]] {

  def repr: Repr

  def size: Int

  def head: T

  /** Selects all elements except the first.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the first one.
   *  @throws `UnsupportedOperationException` if the $coll is empty.
   */
  def tail: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    drop(1)
  }

  /** Computes a prefix scan of the elements of the collection.
   *
   *  Note: The neutral element `z` may be applied more than once.
   *
   *  @tparam U         element type of the resulting collection
   *  @tparam That      type of the resulting collection
   *  @param z          neutral element for the operator `op`
   *  @param op         the associative operator for the scan
   *  @param cbf        combiner factory which provides a combiner
   *  @return           a collection containing the prefix scan of the elements in the original collection
   *
   *  @usecase def scan(z: T)(op: (T, T) => T): $Coll[T]
   *
   *  @return           a new $coll containing the prefix scan of the elements in this $coll
   */
  def scan[U >: T, That](z: U)(op: (U, U) => U)(implicit cbf: CanBuildFrom[Repr, U, That]): That

  def scanLeft[S, That](z: S)(op: (S, T) => S)(implicit bf: CanBuildFrom[Repr, S, That]): That

  def scanRight[S, That](z: S)(op: (T, S) => S)(implicit bf: CanBuildFrom[Repr, S, That]): That

  def foreach[U](f: T => U): Unit

  def map[S, That](f: T => S)(implicit bf: CanBuildFrom[Repr, S, That]): That

  def collect[S, That](pf: PartialFunction[T, S])(implicit bf: CanBuildFrom[Repr, S, That]): That

  def flatMap[S, That](f: T => GenTraversableOnce[S])(implicit bf: CanBuildFrom[Repr, S, That]): That

  def ++[U >: T, That](that: GenTraversableOnce[U])(implicit bf: CanBuildFrom[Repr, U, That]): That

  def filter(pred: T => Boolean): Repr

  def filterNot(pred: T => Boolean): Repr

  def partition(pred: T => Boolean): (Repr, Repr)

  def groupBy[K](f: T => K): GenMap[K, Repr]

  def take(n: Int): Repr

  def drop(n: Int): Repr

  def slice(unc_from: Int, unc_until: Int): Repr

  def splitAt(n: Int): (Repr, Repr)

  def takeWhile(pred: T => Boolean): Repr

  def span(pred: T => Boolean): (Repr, Repr)

  def dropWhile(pred: T => Boolean): Repr

  def copyToArray[U >: T](xs: Array[U], start: Int, len: Int)

  def stringPrefix: String

}
