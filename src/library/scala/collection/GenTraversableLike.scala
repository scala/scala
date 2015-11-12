/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection


import generic._
import scala.annotation.migration


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
 *    Note: might return different results for different runs, unless the
 *    underlying collection type is ordered.
 *  @define orderDependentFold
 *
 *    Note: might return different results for different runs, unless the
 *    underlying collection type is ordered.
 *    or the operator is associative and commutative.
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 *
 *  @define traversableInfo
 *  This is a base trait of all kinds of Scala collections.
 *
 *  @define Coll `GenTraversable`
 *  @define coll general collection
 *  @define collectExample
 *  @tparam A    the collection element type.
 *  @tparam Repr the actual type of the element container.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait GenTraversableLike[+A, +Repr] extends Any with GenTraversableOnce[A] with Parallelizable[A, parallel.ParIterable[A]] {

  def repr: Repr

  def size: Int

  /** Selects the first element of this $coll.
   *  $orderDependent
   *  @return  the first element of this $coll.
   *  @throws NoSuchElementException if the $coll is empty.
   */
  def head: A

  /** Optionally selects the first element.
   *  $orderDependent
   *  @return  the first element of this $coll if it is nonempty,
   *           `None` if it is empty.
   */
  def headOption: Option[A]

  /** Tests whether this $coll can be repeatedly traversed.
   *  @return   `true`
   */
  def isTraversableAgain: Boolean

  /** Selects all elements except the first.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the first one.
   *  @throws UnsupportedOperationException if the $coll is empty.
   */
  def tail: Repr

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A

  /** Optionally selects the last element.
   *  $orderDependent
   *  @return  the last element of this $coll$ if it is nonempty,
   *           `None` if it is empty.
   */
  def lastOption: Option[A]

  /** Selects all elements except the last.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the last one.
   *  @throws UnsupportedOperationException if the $coll is empty.
   */
  def init: Repr

  /** Computes a prefix scan of the elements of the collection.
   *
   *  Note: The neutral element `z` may be applied more than once.
   *
   *  @tparam B         element type of the resulting collection
   *  @tparam That      type of the resulting collection
   *  @param z          neutral element for the operator `op`
   *  @param op         the associative operator for the scan
   *  @param cbf        combiner factory which provides a combiner
   *
   *  @return           a new $coll containing the prefix scan of the elements in this $coll
   */
  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[Repr, B, That]): That

  /** Produces a collection containing cumulative results of applying the
   *  operator going left to right.
   *
   *  $willNotTerminateInf
   *  $orderDependent
   *
   *  @tparam B      the type of the elements in the resulting collection
   *  @tparam That   the actual type of the resulting collection
   *  @param z       the initial value
   *  @param op      the binary operator applied to the intermediate result and the element
   *  @param bf      $bfinfo
   *  @return        collection with intermediate results
   */
  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Produces a collection containing cumulative results of applying the operator going right to left.
   *  The head of the collection is the last cumulative result.
   *  $willNotTerminateInf
   *  $orderDependent
   *
   *  Example:
   *  {{{
   *    List(1, 2, 3, 4).scanRight(0)(_ + _) == List(10, 9, 7, 4, 0)
   *  }}}
   *
   *  @tparam B      the type of the elements in the resulting collection
   *  @tparam That   the actual type of the resulting collection
   *  @param z       the initial value
   *  @param op      the binary operator applied to the intermediate result and the element
   *  @param bf      $bfinfo
   *  @return        collection with intermediate results
   */
  @migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0")
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  def foreach[U](f: A => U): Unit

  /** Builds a new collection by applying a function to all elements of this $coll.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.
   *
   *  @usecase def map[B](f: A => B): $Coll[B]
   *    @inheritdoc
   *    @return       a new $coll resulting from applying the given function
   *                  `f` to each element of this $coll and collecting the results.
   */
  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Builds a new collection by applying a partial function to all elements of this $coll
   *  on which the function is defined.
   *
   *  @param pf     the partial function which filters and maps the $coll.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the partial function
   *                `pf` to each element on which it is defined and collecting the results.
   *                The order of the elements is preserved.
   *
   *  @usecase def collect[B](pf: PartialFunction[A, B]): $Coll[B]
   *    @inheritdoc
   *
   *    $collectExample
   *
   *    @return       a new $coll resulting from applying the given partial function
   *                  `pf` to each element on which it is defined and collecting the results.
   *                  The order of the elements is preserved.
   */
  def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Builds a new collection by applying a function to all elements of this $coll
   *  and using the elements of the resulting collections.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.
   *
   *  @usecase def flatMap[B](f: A => GenTraversableOnce[B]): $Coll[B]
   *    @inheritdoc
   *
   *    For example:
   *
   *    {{{
   *      def getWords(lines: Seq[String]): Seq[String] = lines flatMap (line => line split "\\W+")
   *    }}}
   *
   *    The type of the resulting collection is guided by the static type of $coll. This might
   *    cause unexpected results sometimes. For example:
   *
   *    {{{
   *      // lettersOf will return a Seq[Char] of likely repeated letters, instead of a Set
   *      def lettersOf(words: Seq[String]) = words flatMap (word => word.toSet)
   *
   *      // lettersOf will return a Set[Char], not a Seq
   *      def lettersOf(words: Seq[String]) = words.toSet flatMap (word => word.toSeq)
   *
   *      // xs will be an Iterable[Int]
   *      val xs = Map("a" -> List(11,111), "b" -> List(22,222)).flatMap(_._2)
   *
   *      // ys will be a Map[Int, Int]
   *      val ys = Map("a" -> List(1 -> 11,1 -> 111), "b" -> List(2 -> 22,2 -> 222)).flatMap(_._2)
   *    }}}
   *
   *    @return       a new $coll resulting from applying the given collection-valued function
   *                  `f` to each element of this $coll and concatenating the results.
   */
  def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
   *  right hand operand. The element type of the $coll is the most specific superclass encompassing
   *  the element types of the two operands.
   *
   *  @param that   the traversable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements
   *                of this $coll followed by all elements of `that`.
   *
   *  @usecase def ++[B](that: GenTraversableOnce[B]): $Coll[B]
   *    @inheritdoc
   *
   *    Example:
   *    {{{
   *      scala> val a = List(1)
   *      a: List[Int] = List(1)
   *
   *      scala> val b = List(2)
   *      b: List[Int] = List(2)
   *
   *      scala> val c = a ++ b
   *      c: List[Int] = List(1, 2)
   *
   *      scala> val d = List('a')
   *      d: List[Char] = List(a)
   *
   *      scala> val e = c ++ d
   *      e: List[AnyVal] = List(1, 2, a)
   *    }}}
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Selects all elements of this $coll which satisfy a predicate.
   *
   *  @param pred  the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
   *               predicate `p`. Their order may not be preserved.
   */
  def filter(pred: A => Boolean): Repr

  /** Selects all elements of this $coll which do not satisfy a predicate.
   *
   *  @param pred  the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
   *               predicate `p`. Their order may not be preserved.
   */
  def filterNot(pred: A => Boolean): Repr

  /** Partitions this $coll in two ${coll}s according to a predicate.
   *
   *  @param pred the predicate on which to partition.
   *  @return     a pair of ${coll}s: the first $coll consists of all elements that
   *              satisfy the predicate `p` and the second $coll consists of all elements
   *              that don't. The relative order of the elements in the resulting ${coll}s
   *              may not be preserved.
   */
  def partition(pred: A => Boolean): (Repr, Repr)

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
   *
   *  Note: this method is not re-implemented by views. This means
   *        when applied to a view it will always force the view and
   *        return a new $coll.
   *
   *  @param f     the discriminator function.
   *  @tparam K    the type of keys returned by the discriminator function.
   *  @return      A map from keys to ${coll}s such that the following invariant holds:
   *               {{{
   *                 (xs groupBy f)(k) = xs filter (x => f(x) == k)
   *               }}}
   *               That is, every key `k` is bound to a $coll of those elements `x`
   *               for which `f(x)` equals `k`.
   *
   */
  def groupBy[K](f: A => K): GenMap[K, Repr]

  /** Selects first ''n'' elements.
   *  $orderDependent
   *  @param  n    the number of elements to take from this $coll.
   *  @return a $coll consisting only of the first `n` elements of this $coll,
   *          or else the whole $coll, if it has less than `n` elements.
   */
  def take(n: Int): Repr

  /** Selects all elements except first ''n'' ones.
   *  $orderDependent
   *  @param  n    the number of elements to drop from this $coll.
   *  @return a $coll consisting of all elements of this $coll except the first `n` ones, or else the
   *          empty $coll, if this $coll has less than `n` elements.
   */
  def drop(n: Int): Repr

  /** Selects an interval of elements.  The returned collection is made up
   *  of all elements `x` which satisfy the invariant:
   *  {{{
   *    from <= indexOf(x) < until
   *  }}}
   *  $orderDependent
   *
   *  @param unc_from   the lowest index to include from this $coll.
   *  @param unc_until  the lowest index to EXCLUDE from this $coll.
   *  @return  a $coll containing the elements greater than or equal to
   *           index `from` extending up to (but not including) index `until`
   *           of this $coll.
   */
  def slice(unc_from: Int, unc_until: Int): Repr

  /** Splits this $coll into two at a given position.
   *  Note: `c splitAt n` is equivalent to (but possibly more efficient than)
   *         `(c take n, c drop n)`.
   *  $orderDependent
   *
   *  @param n the position at which to split.
   *  @return  a pair of ${coll}s consisting of the first `n`
   *           elements of this $coll, and the other elements.
   */
  def splitAt(n: Int): (Repr, Repr)

  /** Takes longest prefix of elements that satisfy a predicate.
   *  $orderDependent
   *  @param   pred  The predicate used to test elements.
   *  @return  the longest prefix of this $coll whose elements all satisfy
   *           the predicate `p`.
   */
  def takeWhile(pred: A => Boolean): Repr

  /** Splits this $coll into a prefix/suffix pair according to a predicate.
   *
   *  Note: `c span p`  is equivalent to (but possibly more efficient than)
   *  `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
   *  predicate `p` does not cause any side-effects.
   *  $orderDependent
   *
   *  @param pred the test predicate
   *  @return  a pair consisting of the longest prefix of this $coll whose
   *           elements all satisfy `p`, and the rest of this $coll.
   */
  def span(pred: A => Boolean): (Repr, Repr)

  /** Drops longest prefix of elements that satisfy a predicate.
   *  $orderDependent
   *  @param   pred  The predicate used to test elements.
   *  @return  the longest suffix of this $coll whose first element
   *           does not satisfy the predicate `p`.
   */
  def dropWhile(pred: A => Boolean): Repr

  /** Defines the prefix of this object's `toString` representation.
   *
   *  @return  a string representation which starts the result of `toString`
   *           applied to this $coll. By default the string prefix is the
   *           simple name of the collection class $coll.
   */
  def stringPrefix: String

}
