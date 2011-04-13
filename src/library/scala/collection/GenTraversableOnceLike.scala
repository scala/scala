/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection




/** A template trait for all objects traversable once or more which may possibly
 *  have their traversal occur in parallel.
 *
 *  Methods in this trait are either abstract or can be implemented in terms
 *  of other methods.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *
 *  @define possiblyparinfo
 *  This trait may possibly have operations implemented in parallel.
 */
trait GenTraversableOnceLike[+A] {

  def foreach[U](f: A => U): Unit

  def isEmpty: Boolean

  def hasDefiniteSize: Boolean

  def seq: TraversableOnce[A]

  /** Reduces the elements of this sequence using the specified associative binary operator.
   *
   *  $undefinedorder
   *
   *  Note this method has a different signature than the `reduceLeft`
   *  and `reduceRight` methods of the trait `Traversable`.
   *  The result of reducing may only be a supertype of this parallel collection's
   *  type parameter `T`.
   *
   *  @tparam U      A type parameter for the binary operator, a supertype of `T`.
   *  @param op       A binary operator that must be associative.
   *  @return         The result of applying reduce operator `op` between all the elements if the collection is nonempty.
   *  @throws UnsupportedOperationException
   *  if this $coll is empty.
   */
  def reduce[A1 >: A](op: (A1, A1) => A1): A1

  /** Optionally reduces the elements of this sequence using the specified associative binary operator.
   *
   *  $undefinedorder
   *
   *  Note this method has a different signature than the `reduceLeftOption`
   *  and `reduceRightOption` methods of the trait `Traversable`.
   *  The result of reducing may only be a supertype of this parallel collection's
   *  type parameter `T`.
   *
   *  @tparam U      A type parameter for the binary operator, a supertype of `T`.
   *  @param op      A binary operator that must be associative.
   *  @return        An option value containing result of applying reduce operator `op` between all
   *                 the elements if the collection is nonempty, and `None` otherwise.
   */
  def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1]

  /** Folds the elements of this sequence using the specified associative binary operator.
   *  The order in which the elements are reduced is unspecified and may be nondeterministic.
   *
   *  Note this method has a different signature than the `foldLeft`
   *  and `foldRight` methods of the trait `Traversable`.
   *  The result of folding may only be a supertype of this parallel collection's
   *  type parameter `T`.
   *
   *  @tparam U      a type parameter for the binary operator, a supertype of `T`.
   *  @param z       a neutral element for the fold operation, it may be added to the result
   *                 an arbitrary number of times, not changing the result (e.g. `Nil` for list concatenation,
   *                 0 for addition, or 1 for multiplication)
   *  @param op      a binary operator that must be associative
   *  @return        the result of applying fold operator `op` between all the elements and `z`
   */
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1

  /** A syntactic sugar for out of order folding. See `fold`. */
  def /:\[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = fold(z)(op)

  /** Aggregates the results of applying an operator to subsequent elements.
   *
   *  This is a more general form of `fold` and `reduce`. It has similar semantics, but does
   *  not require the result to be a supertype of the element type. It traverses the elements in
   *  different partitions sequentially, using `seqop` to update the result, and then
   *  applies `combop` to results from different partitions. The implementation of this
   *  operation may operate on an arbitrary number of collection partitions, so `combop`
   *  may be invoked arbitrary number of times.
   *
   *  For example, one might want to process some elements and then produce a `Set`. In this
   *  case, `seqop` would process an element and append it to the list, while `combop`
   *  would concatenate two lists from different partitions together. The initial value
   *  `z` would be an empty set.
   *
   *  {{{
   *    pc.aggregate(Set[Int]())(_ += process(_), _ ++ _)
   *  }}}
   *
   *  Another example is calculating geometric mean from a collection of doubles
   *  (one would typically require big doubles for this).
   *
   *  @tparam S        the type of accumulated results
   *  @param z         the initial value for the accumulated result of the partition - this
   *                   will typically be the neutral element for the `seqop` operator (e.g.
   *                   `Nil` for list concatenation or `0` for summation)
   *  @param seqop     an operator used to accumulate results within a partition
   *  @param combop    an associative operator used to combine results from different partitions
   */
  def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B

  def count(p: A => Boolean): Int

  def sum[A1 >: A](implicit num: Numeric[A1]): A1

  def product[A1 >: A](implicit num: Numeric[A1]): A1

  def min[A1 >: A](implicit ord: Ordering[A1]): A

  def max[A1 >: A](implicit ord: Ordering[A1]): A

  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A

  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A

  def forall(pred: A => Boolean): Boolean

  def exists(pred: A => Boolean): Boolean

  def find(pred: A => Boolean): Option[A]

  def copyToArray[B >: A](xs: Array[B]): Unit

  def copyToArray[B >: A](xs: Array[B], start: Int): Unit

  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  def mkString(start: String, sep: String, end: String): String

  def mkString(sep: String): String

  def mkString: String

  def toArray[A1 >: A: ClassManifest]: Array[A1]

  def toList: List[A]

  def toIndexedSeq[A1 >: A]: immutable.IndexedSeq[A1]

  def toStream: Stream[A]

  def toIterator: Iterator[A]

  def toBuffer[A1 >: A]: collection.mutable.Buffer[A1]

  def toTraversable: GenTraversable[A]

  def toIterable: GenIterable[A]

  def toSeq: GenSeq[A]

  def toSet[A1 >: A]: GenSet[A1]

  def toMap[K, V](implicit ev: A <:< (K, V)): GenMap[K, V]
}
