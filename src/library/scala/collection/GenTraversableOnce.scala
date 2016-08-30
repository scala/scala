/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.language.higherKinds

/** A template trait for all traversable-once objects which may be
 *  traversed in parallel.
 *
 *  Methods in this trait are either abstract or can be implemented in terms
 *  of other methods.
 *
 *  @define Coll `GenTraversableOnce`
 *  @define coll collection or iterator
 *  @define possiblyparinfo
 *  This trait may possibly have operations implemented in parallel.
 *  @define undefinedorder
 *  The order in which operations are performed on elements is unspecified
 *  and may be nondeterministic.
 *  @define orderDependent
 *
 *    Note: might return different results for different runs, unless the
 *    underlying collection type is ordered.
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
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenTraversableOnce[+A] extends Any {

  /** Applies a function `f` to all elements of this $coll.
    *
    *  @param  f   the function that is applied for its side-effect to every element.
    *              The result of function `f` is discarded.
    *
    *  @tparam  U  the type parameter describing the result of function `f`.
    *              This result will always be ignored. Typically `U` is `Unit`,
    *              but this is not necessary.
    *
    *  @usecase def foreach(f: A => Unit): Unit
    *    @inheritdoc
    *
    *    Note: this method underlies the implementation of most other bulk operations.
    *    It's important to implement this method in an efficient way.
    *
    */
  def foreach[U](f: A => U): Unit

  /** Tests whether this $coll is known to have a finite size.
    *  All strict collections are known to have finite size. For a non-strict
    *  collection such as `Stream`, the predicate returns `'''true'''` if all
    *  elements have been computed. It returns `'''false'''` if the stream is
    *  not yet evaluated to the end. Non-empty Iterators usually return
    *  `'''false'''` even if they were created from a collection with a known
    *  finite size.
    *
    *  Note: many collection methods will not work on collections of infinite sizes.
    *  The typical failure mode is an infinite loop. These methods always attempt a
    *  traversal without checking first that `hasDefiniteSize` returns `'''true'''`.
    *  However, checking `hasDefiniteSize` can provide an assurance that size is
    *  well-defined and non-termination is not a concern.
    *
    *  @return  `'''true'''` if this collection is known to have finite size,
    *           `'''false'''` otherwise.
    */
  def hasDefiniteSize: Boolean

  def seq: TraversableOnce[A]

  /** The size of this $coll.
   *
   *  $willNotTerminateInf
   *
   *  @return    the number of elements in this $coll.
   */
  def size: Int

   /** The size of this $coll if it is can be cheaply computed
   *
   *  @return    the number of elements in this $coll, or -1 if the size cannot be determined cheaply
   */
  protected[collection] def sizeHintIfCheap: Int = -1

  /** Tests whether the $coll is empty.
   *
   *  Note: Implementations in subclasses that are not repeatedly traversable must take
   *  care not to consume any elements when `isEmpty` is called.
   *
   *  @return    `true` if the $coll contains no elements, `false` otherwise.
   */
  def isEmpty: Boolean

  /** Tests whether the $coll is not empty.
   *
   *  @return    `true` if the $coll contains at least one element, `false` otherwise.
   */
  def nonEmpty: Boolean

  /** Tests whether this $coll can be repeatedly traversed.  Always
   *  true for Traversables and false for Iterators unless overridden.
   *
   *  @return   `true` if it is repeatedly traversable, `false` otherwise.
   */
  def isTraversableAgain: Boolean

  /** Reduces the elements of this $coll using the specified associative binary operator.
   *
   *  $undefinedorder
   *
   *  @tparam A1      A type parameter for the binary operator, a supertype of `A`.
   *  @param op       A binary operator that must be associative.
   *  @return         The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
   *  @throws UnsupportedOperationException
   *  if this $coll is empty.
   */
  def reduce[A1 >: A](op: (A1, A1) => A1): A1

  /** Reduces the elements of this $coll, if any, using the specified
   *  associative binary operator.
   *
   *  $undefinedorder
   *
   *  @tparam A1     A type parameter for the binary operator, a supertype of `A`.
   *  @param op      A binary operator that must be associative.
   *  @return        An option value containing result of applying reduce operator `op` between all
   *                 the elements if the collection is nonempty, and `None` otherwise.
   */
  def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1]

  /** Folds the elements of this $coll using the specified associative
   *  binary operator.
   *
   *  $undefinedorder
   *  $willNotTerminateInf
   *
   *  @tparam A1     a type parameter for the binary operator, a supertype of `A`.
   *  @param z       a neutral element for the fold operation; may be added to the result
   *                 an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
   *                 0 for addition, or 1 for multiplication).
   *  @param op      a binary operator that must be associative.
   *  @return        the result of applying the fold operator `op` between all the elements and `z`, or `z` if this $coll is empty.
   */
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1

  /** Applies a binary operator to a start value and all elements of this $coll,
   *  going left to right.
   *
   *  Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as
   *  `xs foldLeft z`.
   *
   *  Examples:
   *
   *  Note that the folding function used to compute b is equivalent to that used to compute c.
   *  {{{
   *      scala> val a = List(1,2,3,4)
   *      a: List[Int] = List(1, 2, 3, 4)
   *
   *      scala> val b = (5 /: a)(_+_)
   *      b: Int = 15
   *
   *      scala> val c = (5 /: a)((x,y) => x + y)
   *      c: Int = 15
   *  }}}

   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll,
   *           going left to right with the start value `z` on the left:
   *           {{{
   *             op(...op(op(z, x_1), x_2), ..., x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   */
  def /:[B](z: B)(op: (B, A) => B): B

  /** Applies a binary operator to all elements of this $coll and a start value,
   *  going right to left.
   *
   *  Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as
   *  `xs foldRight z`.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  Examples:
   *
   *  Note that the folding function used to compute b is equivalent to that used to compute c.
   *  {{{
   *      scala> val a = List(1,2,3,4)
   *      a: List[Int] = List(1, 2, 3, 4)
   *
   *      scala> val b = (a :\ 5)(_+_)
   *      b: Int = 15
   *
   *      scala> val c = (a :\ 5)((x,y) => x + y)
   *      c: Int = 15
   *
   *  }}}
   *
   *  @param   z    the start value
   *  @param   op   the binary operator
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll,
   *           going right to left with the start value `z` on the right:
   *           {{{
   *             op(x_1, op(x_2, ... op(x_n, z)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   */
  def :\[B](z: B)(op: (A, B) => B): B

  /** Applies a binary operator to a start value and all elements of this $coll,
   *  going left to right.
   *
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll,
   *           going left to right with the start value `z` on the left:
   *           {{{
   *             op(...op(z, x_1), x_2, ..., x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   *           Returns `z` if this $coll is empty.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B

  /** Applies a binary operator to all elements of this $coll and a start value,
   *  going right to left.
   *
   *  $willNotTerminateInf
   *  $orderDependentFold
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll,
   *           going right to left with the start value `z` on the right:
   *           {{{
   *             op(x_1, op(x_2, ... op(x_n, z)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   *           Returns `z` if this $coll is empty.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B

  /** Aggregates the results of applying an operator to subsequent elements.
   *
   *  This is a more general form of `fold` and `reduce`. It is similar to
   *  `foldLeft` in that it doesn't require the result to be a supertype of the
   *  element type. In addition, it allows parallel collections to be processed
   *  in chunks, and then combines the intermediate results.
   *
   *  `aggregate` splits the $coll into partitions and processes each
   *  partition by sequentially applying `seqop`, starting with `z` (like
   *  `foldLeft`). Those intermediate results are then combined by using
   *  `combop` (like `fold`). The implementation of this operation may operate
   *  on an arbitrary number of collection partitions (even 1), so `combop` may
   *  be invoked an arbitrary number of times (even 0).
   *
   *  As an example, consider summing up the integer values of a list of chars.
   *  The initial value for the sum is 0. First, `seqop` transforms each input
   *  character to an Int and adds it to the sum (of the partition). Then,
   *  `combop` just needs to sum up the intermediate results of the partitions:
   *  {{{
   *    List('a', 'b', 'c').aggregate(0)({ (sum, ch) => sum + ch.toInt }, { (p1, p2) => p1 + p2 })
   *  }}}
   *
   *  @tparam B        the type of accumulated results
   *  @param z         the initial value for the accumulated result of the partition - this
   *                   will typically be the neutral element for the `seqop` operator (e.g.
   *                   `Nil` for list concatenation or `0` for summation) and may be evaluated
   *                   more than once
   *  @param seqop     an operator used to accumulate results within a partition
   *  @param combop    an associative operator used to combine results from different partitions
   */
  def aggregate[B](z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B

  /** Applies a binary operator to all elements of this $coll, going right to left.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll,
   *           going right to left:
   *           {{{
   *             op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   *  @throws UnsupportedOperationException if this $coll is empty.
   */
  def reduceRight[B >: A](op: (A, B) => B): B

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  an option value containing the result of `reduceLeft(op)` if this $coll is nonempty,
   *           `None` otherwise.
   */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B]

  /** Optionally applies a binary operator to all elements of this $coll, going
   *  right to left.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  an option value containing the result of `reduceRight(op)` if this $coll is nonempty,
   *           `None` otherwise.
   */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B]

  /** Counts the number of elements in the $coll which satisfy a predicate.
   *
   *  @param p     the predicate  used to test elements.
   *  @return      the number of elements satisfying the predicate `p`.
   */
  def count(p: A => Boolean): Int

  /** Sums up the elements of this collection.
   *
   *   @param   num  an implicit parameter defining a set of numeric operations
   *                 which includes the `+` operator to be used in forming the sum.
   *   @tparam  A1   the result type of the `+` operator.
   *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
   *
   *   @usecase def sum: A
   *     @inheritdoc
   *
   *     @return       the sum of all elements in this $coll of numbers of type `Int`.
   *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
   *     can be used as element type of the $coll and as result type of `sum`.
   *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
   *
   */
  def sum[A1 >: A](implicit num: Numeric[A1]): A1

  /** Multiplies up the elements of this collection.
   *
   *   @param   num  an implicit parameter defining a set of numeric operations
   *                 which includes the `*` operator to be used in forming the product.
   *   @tparam  A1   the result type of the `*` operator.
   *   @return       the product of all elements of this $coll with respect to the `*` operator in `num`.
   *
   *   @usecase def product: A
   *     @inheritdoc
   *
   *     @return       the product of all elements in this $coll of numbers of type `Int`.
   *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
   *     can be used as element type of the $coll and as result type of `product`.
   *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
   */
  def product[A1 >: A](implicit num: Numeric[A1]): A1

  /** Finds the smallest element.
   *
   *  @param    ord   An ordering to be used for comparing elements.
   *  @tparam   A1    The type over which the ordering is defined.
   *  @return   the smallest element of this $coll with respect to the ordering `ord`.
   *
   *  @usecase def min: A
   *    @inheritdoc
   *
   *    @return   the smallest element of this $coll
   */
  def min[A1 >: A](implicit ord: Ordering[A1]): A

  /** Finds the largest element.
   *
   *  @param    ord   An ordering to be used for comparing elements.
   *  @tparam   A1    The type over which the ordering is defined.
   *  @return   the largest element of this $coll with respect to the ordering `ord`.
   *
   *  @usecase def max: A
   *    @inheritdoc
   *
   *    @return   the largest element of this $coll.
   */
  def max[A1 >: A](implicit ord: Ordering[A1]): A

  /** Finds the first element which yields the largest value measured by function f.
   *
   *  @param    cmp   An ordering to be used for comparing elements.
   *  @tparam   B     The result type of the function f.
   *  @param    f     The measuring function.
   *  @return   the first element of this $coll with the largest value measured by function f
   *  with respect to the ordering `cmp`.
   *
   *  @usecase def maxBy[B](f: A => B): A
   *    @inheritdoc
   *
   *    @return   the first element of this $coll with the largest value measured by function f.
   */
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A

  /** Finds the first element which yields the smallest value measured by function f.
   *
   *  @param    cmp   An ordering to be used for comparing elements.
   *  @tparam   B     The result type of the function f.
   *  @param    f     The measuring function.
   *  @return   the first element of this $coll with the smallest value measured by function f
   *  with respect to the ordering `cmp`.
   *
   *  @usecase def minBy[B](f: A => B): A
   *    @inheritdoc
   *
   *    @return   the first element of this $coll with the smallest value measured by function f.
   */
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if this $coll is empty or the given predicate `p`
   *                 holds for all elements of this $coll, otherwise `false`.
   */
  def forall(@deprecatedName('pred) p: A => Boolean): Boolean

  /** Tests whether a predicate holds for at least one element of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` is satisfied by at least one element of this $coll, otherwise `false`
   */
  def exists(@deprecatedName('pred) p: A => Boolean): Boolean

  /** Finds the first element of the $coll satisfying a predicate, if any.
   *
   *  $mayNotTerminateInf
   *  $orderDependent
   *
   *  @param p       the predicate used to test elements.
   *  @return        an option value containing the first element in the $coll
   *                 that satisfies `p`, or `None` if none exists.
   */
  def find(@deprecatedName('pred) p: A => Boolean): Option[A]

  /** Copies the elements of this $coll to an array.
   *  Fills the given array `xs` with values of this $coll.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the target array is reached.
   *
   *  @param  xs     the array to fill.
   *  @tparam B      the type of the elements of the target array.
   *
   *  @usecase def copyToArray(xs: Array[A]): Unit
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   */
  def copyToArray[B >: A](xs: Array[B]): Unit

  /** Copies the elements of this $coll to an array.
   *  Fills the given array `xs` with values of this $coll, beginning at index `start`.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the target array is reached.
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @tparam B      the type of the elements of the target array.
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int): Unit
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit

  /** Copies the elements of this $coll to an array.
   *  Fills the given array `xs` with at most `len` elements of
   *  this $coll, starting at position `start`.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the target array is reached, or `len` elements have been copied.
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @param  len    the maximal number of elements to copy.
   *  @tparam B      the type of the elements of the target array.
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  /** Displays all elements of this $coll in a string using start, end, and
   *  separator strings.
   *
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      a string representation of this $coll. The resulting string
   *               begins with the string `start` and ends with the string
   *               `end`. Inside, the string representations (w.r.t. the method
   *               `toString`) of all elements of this $coll are separated by
   *               the string `sep`.
   *
   *  @example  `List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
   */
  def mkString(start: String, sep: String, end: String): String

  /** Displays all elements of this $coll in a string using a separator string.
   *
   *  @param sep   the separator string.
   *  @return      a string representation of this $coll. In the resulting string
   *               the string representations (w.r.t. the method `toString`)
   *               of all elements of this $coll are separated by the string `sep`.
   *
   *  @example  `List(1, 2, 3).mkString("|") = "1|2|3"`
   */
  def mkString(sep: String): String

  /** Displays all elements of this $coll in a string.
   *
   *  @return a string representation of this $coll. In the resulting string
   *          the string representations (w.r.t. the method `toString`)
   *          of all elements of this $coll follow each other without any
   *          separator string.
   */
  def mkString: String

  /** Converts this $coll to an array.
   *
   *  @tparam A1 the type of the elements of the array. An `ClassTag` for
   *             this type must be available.
   *  @return    an array containing all elements of this $coll.
   *
   *  @usecase def toArray: Array[A]
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    @return  an array containing all elements of this $coll.
   *             An `ClassTag` must be available for the element type of this $coll.
   */
  def toArray[A1 >: A: ClassTag]: Array[A1]

  /** Converts this $coll to a list.
   *  $willNotTerminateInf
   *  @return a list containing all elements of this $coll.
   */
  def toList: List[A]

  /** Converts this $coll to an indexed sequence.
   *  $willNotTerminateInf
   *  @return an indexed sequence containing all elements of this $coll.
   */
  def toIndexedSeq: immutable.IndexedSeq[A]

  /** Converts this $coll to a stream.
   *  @return a stream containing all elements of this $coll.
   */
  def toStream: Stream[A]

  /** Returns an Iterator over the elements in this $coll.  Will return
   *  the same Iterator if this instance is already an Iterator.
   *  $willNotTerminateInf
   *  @return an Iterator containing all elements of this $coll.
   */
  def toIterator: Iterator[A]

  /** Uses the contents of this $coll to create a new mutable buffer.
   *  $willNotTerminateInf
   *  @return a buffer containing all elements of this $coll.
   */
  def toBuffer[A1 >: A]: scala.collection.mutable.Buffer[A1]

  /** Converts this $coll to an unspecified Traversable.  Will return
   *  the same collection if this instance is already Traversable.
   *  $willNotTerminateInf
   *  @return a Traversable containing all elements of this $coll.
   */
  def toTraversable: GenTraversable[A]

  /** Converts this $coll to an iterable collection.  Note that
   *  the choice of target `Iterable` is lazy in this default implementation
   *  as this `TraversableOnce` may be lazy and unevaluated (i.e. it may
   *  be an iterator which is only traversable once).
   *
   *  $willNotTerminateInf
   *  @return an `Iterable` containing all elements of this $coll.
   */
  def toIterable: GenIterable[A]

  /** Converts this $coll to a sequence. As with `toIterable`, it's lazy
   *  in this default implementation, as this `TraversableOnce` may be
   *  lazy and unevaluated.
   *
   *  $willNotTerminateInf
   *  @return a sequence containing all elements of this $coll.
   */
  def toSeq: GenSeq[A]

  /** Converts this $coll to a set.
   *  $willNotTerminateInf
   *  @return      a set containing all elements of this $coll.
   */
  def toSet[A1 >: A]: GenSet[A1]

  /** Converts this $coll to a map.  This method is unavailable unless
   *  the elements are members of Tuple2, each ((T, U)) becoming a key-value
   *  pair in the map.  Duplicate keys will be overwritten by later keys:
   *  if this is an unordered collection, which key is in the resulting map
   *  is undefined.
   *  @return    a map containing all elements of this $coll.
   *
   *  @usecase   def toMap[T, U]: Map[T, U]
   *    @inheritdoc
   *    $willNotTerminateInf
   *    @return    a map of type `immutable.Map[T, U]`
   *               containing all key/value pairs of type `(T, U)` of this $coll.
   */
  def toMap[K, V](implicit ev: A <:< (K, V)): GenMap[K, V]

  /** Converts this $coll to a Vector.
   *  $willNotTerminateInf
   *  @return a vector containing all elements of this $coll.
   */
  def toVector: Vector[A]

  /** Converts this $coll into another by copying all elements.
   *  @tparam Col  The collection type to build.
   *  @return a new collection containing all elements of this $coll.
   *
   *  @usecase def to[Col[_]]: Col[A]
   *    @inheritdoc
   *    $willNotTerminateInf
   *    @return a new collection containing all elements of this $coll.
   */
  def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A @uV]]): Col[A @uV]
}
