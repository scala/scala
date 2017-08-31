package strawman
package collection

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag
import scala.{Any, AnyRef, Array, Boolean, `inline`, Int, None, Numeric, Option, Ordering, PartialFunction, StringContext, Some, Unit}
import java.lang.{String, UnsupportedOperationException}
import scala.Predef.<:<

import strawman.collection.mutable.{ArrayBuffer, Builder, StringBuilder}
import java.lang.String

/** Base trait for generic collections */
trait Iterable[+A] extends IterableOnce[A] with IterableOps[A, Iterable, Iterable[A]] {

  /** The collection itself */
  final def toIterable: this.type = this

}

/** Base trait for Iterable operations
  *
  *  VarianceNote
  *  ============
  *
  *  We require that for all child classes of Iterable the variance of
  *  the child class and the variance of the `C` parameter passed to `IterableOps`
  *  are the same. We cannot express this since we lack variance polymorphism. That's
  *  why we have to resort at some places to write `C[A @uncheckedVariance]`.
  *
  *  @tparam CC type constructor of the collection (e.g. `List`, `Set`). Operations returning a collection
  *             with a different type of element `B` (e.g. `map`) return a `CC[B]`.
  *  @tparam C  type of the collection (e.g. `List[Int]`, `String`, `BitSet`). Operations returning a collection
  *             with the same type of element (e.g. `drop`, `filter`) return a `C`.
  */
trait IterableOps[+A, +CC[X], +C] extends Any {

  /**
    * @return This collection as an `Iterable[A]`. No new collection will be built if `this` is already an `Iterable[A]`.
    */
  def toIterable: Iterable[A]

  /**
    * @return This collection as a `C`.
    */
  protected[this] def coll: C

  protected[this] def fromSpecificIterable(coll: Iterable[A]): C

  protected[this] def fromIterable[E](it: Iterable[E]): CC[E] = iterableFactory.fromIterable(it)

  def iterableFactory: IterableFactory[CC]

  /**
    * @return a strict builder for the same collection type.
    *
    * Note that in the case of lazy collections (e.g. [[View]] or [[immutable.LazyList]]),
    * it is possible to implement this method but the resulting `Builder` will break laziness.
    * As a consequence, operations should preferably be implemented on top of views rather
    * than builders.
    */
  protected[this] def newSpecificBuilder(): Builder[A, C]

  // Consumes all the collection!
  protected[this] def reversed: Iterable[A] = {
    var xs: immutable.List[A] = immutable.Nil
    val it = toIterable.iterator()
    while (it.hasNext) xs = it.next() :: xs
    xs
  }

  /** Apply `f` to each element for its side effects
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreach[U](f: A => U): Unit = toIterable.iterator().foreach(f)

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if this $coll is empty or the given predicate `p`
   *                 holds for all elements of this $coll, otherwise `false`.
   */
  def forall(p: A => Boolean): Boolean = toIterable.iterator().forall(p)

  /** Tests whether a predicate holds for at least one element of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` is satisfied by at least one element of this $coll, otherwise `false`
   */
  def exists(p: A => Boolean): Boolean = toIterable.iterator().exists(p)

  /** Counts the number of elements in the $coll which satisfy a predicate.
   *
   *  @param p     the predicate  used to test elements.
   *  @return      the number of elements satisfying the predicate `p`.
   */
  def count(p: A => Boolean): Int = toIterable.iterator().count(p)

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  $mayNotTerminateInf
    *  $orderDependent
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(p: A => Boolean): Option[A] = toIterable.iterator().find(p)

  /** Fold left */
  def foldLeft[B](z: B)(op: (B, A) => B): B = toIterable.iterator().foldLeft(z)(op)

  /** Fold right */
  def foldRight[B](z: B)(op: (A, B) => B): B = toIterable.iterator().foldRight(z)(op)

  /** Reduces the elements of this $coll using the specified associative binary operator.
   *
   *  $undefinedorder
   *
   *  @tparam B      A type parameter for the binary operator, a supertype of `A`.
   *  @param op       A binary operator that must be associative.
   *  @return         The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
   *  @throws UnsupportedOperationException
   *  if this $coll is empty.
   */
  def reduce[B >: A](op: (B, B) => B): B = reduceLeft(op)

  /** Reduces the elements of this $coll, if any, using the specified
   *  associative binary operator.
   *
   *  $undefinedorder
   *
   *  @tparam B     A type parameter for the binary operator, a supertype of `A`.
   *  @param op      A binary operator that must be associative.
   *  @return        An option value containing result of applying reduce operator `op` between all
   *                 the elements if the collection is nonempty, and `None` otherwise.
   */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = reduceLeftOption(op)

  /** Applies a binary operator to all elements of this $coll,
   *  going left to right.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll,
   *           going left to right:
   *           {{{
   *             op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   *  @throws UnsupportedOperationException if this $coll is empty.   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = 0.asInstanceOf[B]

    for (x <- toIterable) {
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }

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
  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceRight")

    reversed.reduceLeft[B]((x, y) => op(y, x))
  }

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  an option value containing the result of `reduceLeft(op)` if this $coll is nonempty,
   *           `None` otherwise.
   */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = if (isEmpty) None else Some(reduceLeft(op))

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
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = if (isEmpty) None else Some(reduceRight(op))

  /** Is the collection empty? */
  def isEmpty: Boolean = !toIterable.iterator().hasNext

  /** Is the collection not empty? */
  def nonEmpty: Boolean = toIterable.iterator().hasNext

  /** The first element of the collection. */
  def head: A = toIterable.iterator().next()

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    val it = toIterable.iterator()
    var lst = it.next()
    while (it.hasNext) lst = it.next()
    lst
  }

  /** Optionally selects the last element.
    *  $orderDependent
    *  @return  the last element of this $coll$ if it is nonempty,
    *           `None` if it is empty.
    */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** The number of elements in this collection, if it can be cheaply computed,
    *  -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
    */
  def knownSize: Int = -1

  /** The number of elements in this collection. Does not terminate for
    *  infinite collections.
    */
  def size: Int = if (knownSize >= 0) knownSize else toIterable.iterator().length

  /** A view representing the elements of this collection. */
  def view: View[A] = View.fromIterator(toIterable.iterator())

  /** Given a collection factory `fi`, convert this collection to the appropriate
    * representation for the current element type `A`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    *      xs.to(BitSet) // for xs: Iterable[Int]
    */
  def to[C1](f: CanBuild[A, C1]): C1 = f.fromSpecificIterable(toIterable)

  /** Convert collection to array. */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
    else ArrayBuffer.fromIterable(toIterable).toArray[B]

  /** Copy all elements of this collection to array `xs`, starting at `start`. */
  def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
    var i = start
    val it = toIterable.iterator()
    while (it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    xs
  }

  /** The class name of this collection. To be used for converting to string.
    *  Collections generally print like this:
    *
    *       <className>(elem_1, ..., elem_n)
    */
  def className = getClass.getName

  /** A string showing all elements of this collection, separated by string `sep`. */
  def mkString(start: String, sep: String, end: String): String = {
    var first: Boolean = true
    val b = new StringBuilder()
    b ++= start
    foreach { elem =>
      if (!first) b ++= sep
      first = false
      b ++= String.valueOf(elem)
    }
    b ++= end
    b.result()
  }

  def mkString(sep: String): String = mkString("", sep, "")

  def mkString: String = mkString("")

  override def toString = s"$className(${mkString(", ")})"


  /** Sums up the elements of this collection.
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `+` operator to be used in forming the sum.
    *   @tparam  B    the result type of the `+` operator.
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
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Multiplies up the elements of this collection.
   *
   *   @param   num  an implicit parameter defining a set of numeric operations
   *                 which includes the `*` operator to be used in forming the product.
   *   @tparam  B   the result type of the `*` operator.
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
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Finds the smallest element.
   *
   *  @param    ord   An ordering to be used for comparing elements.
   *  @tparam   B    The type over which the ordering is defined.
   *  @return   the smallest element of this $coll with respect to the ordering `ord`.
   *
   *  @usecase def min: A
   *    @inheritdoc
   *
   *    @return   the smallest element of this $coll
   */
  def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")

    reduceLeft((x, y) => if (ord.lteq(x, y)) x else y)
  }

  /** Finds the largest element.
   *
   *  @param    ord   An ordering to be used for comparing elements.
   *  @tparam   B    The type over which the ordering is defined.
   *  @return   the largest element of this $coll with respect to the ordering `ord`.
   *
   *  @usecase def max: A
   *    @inheritdoc
   *
   *    @return   the largest element of this $coll.
   */
  def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")

    reduceLeft((x, y) => if (ord.gteq(x, y)) x else y)
  }

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
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")

    var maxF: B = null.asInstanceOf[B]
    var maxElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- toIterable) {
      val fx = f(elem)
      if (first || cmp.gt(fx, maxF)) {
        maxElem = elem
        maxF = fx
        first = false
      }
    }
    maxElem
  }

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
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    var minF: B = null.asInstanceOf[B]
    var minElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- toIterable) {
      val fx = f(elem)
      if (first || cmp.lt(fx, minF)) {
        minElem = elem
        minF = fx
        first = false
      }
    }
    minElem
  }

  /** Selects all elements of this $coll which satisfy a predicate.
    *
    *  @param pred  the predicate used to test elements.
    *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
    *               predicate `pred`. Their order may not be preserved.
    */
  def filter(pred: A => Boolean): C = fromSpecificIterable(View.Filter(toIterable, pred, isFlipped = false))

  /** Selects all elements of this $coll which do not satisfy a predicate.
    *
    *  @param pred  the predicate used to test elements.
    *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
    *               predicate `pred`. Their order may not be preserved.
    */
  def filterNot(pred: A => Boolean): C = fromSpecificIterable(View.Filter(toIterable, pred, isFlipped = true))

  /** Creates a non-strict filter of this $coll.
    *
    *  Note: the difference between `c filter p` and `c withFilter p` is that
    *        the former creates a new collection, whereas the latter only
    *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
    *        and `withFilter` operations.
    *  $orderDependent
    *
    *  @param p   the predicate used to test elements.
    *  @return    an object of class `WithFilter`, which supports
    *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
    *             All these operations apply to those elements of this $coll
    *             which satisfy the predicate `p`.
    */
  def withFilter(p: A => Boolean): collection.WithFilter[A, CC] = new WithFilter(p)

  /** A template trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
    *  of trait `Iterable`.
    */
  class WithFilter(p: A => Boolean) extends collection.WithFilter[A, CC] {

    protected[this] def filtered = View.Filter(toIterable, p, isFlipped = false)

    def map[B](f: A => B): CC[B] = iterableFactory.fromIterable(View.Map(filtered, f))

    def flatMap[B](f: A => IterableOnce[B]): CC[B] = iterableFactory.fromIterable(View.FlatMap(filtered, f))

    def foreach[U](f: A => U): Unit = filtered.foreach(f)

    def withFilter(q: A => Boolean): WithFilter = new WithFilter(a => p(a) && q(a))

  }

  /** A pair of, first, all elements that satisfy prediacte `p` and, second,
    *  all elements that do not. Interesting because it splits a collection in two.
    *
    *  The default implementation provided here needs to traverse the collection twice.
    *  Strict collections have an overridden version of `partition` in `Buildable`,
    *  which requires only a single traversal.
    */
  def partition(p: A => Boolean): (C, C) = {
    val pn = View.Partition(toIterable, p)
    (fromSpecificIterable(pn.first), fromSpecificIterable(pn.second))
  }

  /** Splits this $coll into two at a given position.
    *  Note: `c splitAt n` is equivalent to (but possibly more efficient than)
    *         `(c take n, c drop n)`.
    *  $orderDependent
    *
    *  @param n the position at which to split.
    *  @return  a pair of ${coll}s consisting of the first `n`
    *           elements of this $coll, and the other elements.
    */
  def splitAt(n: Int): (C, C) = (take(n), drop(n))

  /** A collection containing the first `n` elements of this collection. */
  def take(n: Int): C = fromSpecificIterable(View.Take(toIterable, n))

  /** A collection containing the last `n` elements of this collection. */
  def takeRight(n: Int): C = {
    val b = newSpecificBuilder()
    b.sizeHintBounded(n, toIterable)
    val lead = toIterable.iterator() drop n
    val it = toIterable.iterator()
    while (lead.hasNext) {
      lead.next()
      it.next()
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  /** Takes longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest prefix of this $coll whose elements all satisfy
    *           the predicate `p`.
    */
  def takeWhile(p: A => Boolean): C = fromSpecificIterable(View.TakeWhile(toIterable, p))

  /** Splits this $coll into a prefix/suffix pair according to a predicate.
    *
    *  Note: `c span p`  is equivalent to (but possibly more efficient than)
    *  `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
    *  predicate `p` does not cause any side-effects.
    *  $orderDependent
    *
    *  @param p the test predicate
    *  @return  a pair consisting of the longest prefix of this $coll whose
    *           elements all satisfy `p`, and the rest of this $coll.
    */
  def span(p: A => Boolean): (C, C) = (takeWhile(p), dropWhile(p))

  /** The rest of the collection without its `n` first elements. For
    *  linear, immutable collections this should avoid making a copy.
    */
  def drop(n: Int): C = fromSpecificIterable(View.Drop(toIterable, n))

  /** The rest of the collection without its `n` last elements. For
    *  linear, immutable collections this should avoid making a copy.
    */
  def dropRight(n: Int): C = {
    val b = newSpecificBuilder()
    if (n >= 0) b.sizeHint(toIterable, delta = -n)
    val lead = toIterable.iterator() drop n
    val it = toIterable.iterator()
    while (lead.hasNext) {
      b += it.next()
      lead.next()
    }
    b.result()
  }

  /** Skips longest sequence of elements of this iterator which satisfy given
    *  predicate `p`, and returns an iterator of the remaining elements.
    *
    *  @param p the predicate used to skip elements.
    *  @return  an iterator consisting of the remaining elements
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def dropWhile(p: A => Boolean): C = fromSpecificIterable(View.DropWhile(toIterable, p))

  /** Partitions elements in fixed size ${coll}s.
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be less than size `size` if the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[C] =
    toIterable.iterator().grouped(size).map(fromSpecificIterable)

  /** Groups elements in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in `grouped`.)
    *  The "sliding window" step is set to one.
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of elements per group
    *  @return An iterator producing ${coll}s of size `size`, except the
    *          last element (which may be the only element) will be truncated
    *          if there are fewer than `size` elements remaining to be grouped.
    */
  def sliding(size: Int): Iterator[C] = sliding(size, 1)

  /** Groups elements in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in grouped.)
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of elements per group
    *  @param step the distance between the first elements of successive
    *         groups
    *  @return An iterator producing ${coll}s of size `size`, except the
    *          last element (which may be the only element) will be truncated
    *          if there are fewer than `size` elements remaining to be grouped.
    */
  def sliding(size: Int, step: Int): Iterator[C] =
    toIterable.iterator().sliding(size, step).map(fromSpecificIterable)

  /** The rest of the collection without its first element. */
  def tail: C = {
    if (toIterable.isEmpty) throw new UnsupportedOperationException
    drop(1)
  }

  /** The initial part of the collection without its last element. */
  def init: C = {
    if (toIterable.isEmpty) throw new UnsupportedOperationException
    dropRight(1)
  }

  /** Selects an interval of elements.  The returned collection is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  $orderDependent
    *
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    *  @return  a $coll containing the elements greater than or equal to
    *           index `from` extending up to (but not including) index `until`
    *           of this $coll.
    */
  def slice(from: Int, until: Int): C =
    fromSpecificIterable(View.Drop(View.Take(toIterable, until), from))

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    *
    *  Note: When applied to a view or a lazy collection it will always force the elements.
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
  def groupBy[K](f: A => K): immutable.Map[K, C] = {
    val m = mutable.Map.empty[K, Builder[A, C]]
    for (elem <- toIterable) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, newSpecificBuilder())
      bldr += elem
    }
    var result = immutable.Map.empty[K, C]
    m.foreach { case (k, v) =>
      result = result + ((k, v.result()))
    }
    result
  }

  /** Computes a prefix scan of the elements of the collection.
    *
    *  Note: The neutral element `z` may be applied more than once.
    *
    *  @tparam B         element type of the resulting collection
    *  @param z          neutral element for the operator `op`
    *  @param op         the associative operator for the scan
    *
    *  @return           a new $coll containing the prefix scan of the elements in this $coll
    */
  def scan[B >: A](z: B)(op: (B, B) => B): CC[B] = scanLeft(z)(op)

  /** Produces a collection containing cumulative results of applying the
    *  operator going left to right.
    *
    *  $willNotTerminateInf
    *  $orderDependent
    *
    *  @tparam B      the type of the elements in the resulting collection
    *  @param z       the initial value
    *  @param op      the binary operator applied to the intermediate result and the element
    *  @return        collection with intermediate results
    */
  def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = fromIterable(View.ScanLeft(toIterable, z, op))

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
    *  @param z       the initial value
    *  @param op      the binary operator applied to the intermediate result and the element
    *  @return        collection with intermediate results
    */
  def scanRight[B](z: B)(op: (A, B) => B): CC[B] = {
    var scanned = z :: immutable.Nil
    var acc = z
    for (x <- reversed) {
      acc = op(x, acc)
      scanned ::= acc
    }
    fromIterable(scanned)
  }

  /** Builds a new collection by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[B](f: A => B): CC[B] = fromIterable(View.Map(toIterable, f))

  /** Flatmap */
  def flatMap[B](f: A => IterableOnce[B]): CC[B] = fromIterable(View.FlatMap(toIterable, f))

  def flatten[B](implicit ev: A => IterableOnce[B]): CC[B] =
    fromIterable(View.FlatMap(toIterable, ev))

  def collect[B](pf: PartialFunction[A, B]): CC[B] =
    flatMap { a =>
      if (pf.isDefinedAt(a)) View.Single(pf(a))
      else View.Empty
    }

  /** Finds the first element of the $coll for which the given partial
    *  function is defined, and applies the partial function to it.
    *
    *  $mayNotTerminateInf
    *  $orderDependent
    *
    *  @param pf   the partial function
    *  @return     an option value containing pf applied to the first
    *              value for which it is defined, or `None` if none exists.
    *  @example    `Seq("a", 1, 5L).collectFirst({ case x: Int => x*10 }) = Some(10)`
    */
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    val i: Iterator[A] = toIterable.iterator()
    // Presumably the fastest way to get in and out of a partial function is for a sentinel function to return itself
    // (Tested to be lower-overhead than runWith.  Would be better yet to not need to (formally) allocate it)
    val sentinel: scala.Function1[A, Any] = new scala.runtime.AbstractFunction1[A, Any]{ def apply(a: A) = this }
    while (i.hasNext) {
      val x = pf.applyOrElse(i.next(), sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) return Some(x.asInstanceOf[B])
    }
    None
  }

  /** Alias for `appendAll` */
  @`inline` final def concat[B >: A](suffix: Iterable[B]): CC[B] = appendAll(suffix)

  /** Alias for `appendAll` */
  @`inline` final def ++ [B >: A](suffix: Iterable[B]): CC[B] = appendAll(suffix)

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix the traversable to append.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection of type `CC[B]` which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def appendAll[B >: A](suffix: Iterable[B]): CC[B] = fromIterable(View.Concat(toIterable, suffix))

  /** Alias for `appendAll` */
  @`inline` final def :++ [B >: A](suffix: Iterable[B]): CC[B] = appendAll(suffix)

  /** As with `:++`, returns a new collection containing the elements from the left operand followed by the
    *  elements from the right operand.
    *
    *  It differs from `:++` in that the right operand determines the type of
    *  the resulting collection rather than the left one.
    *  Mnemonic: the COLon is on the side of the new COLlection type.
    *
    *  @param prefix   the traversable to prepend.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection which contains all elements
    *                of `prefix` followed by all the elements of this $coll.
    *
    *  @usecase def prependAll[B](that: TraversableOnce[B]): $Coll[B]
    *    @inheritdoc
    *
    *    Example:
    *    {{{
    *      scala> val x = List(1)
    *      x: List[Int] = List(1)
    *
    *      scala> val y = Vector(2)
    *      y: scala.collection.immutable.Vector[Int] = Vector(2)
    *
    *      scala> val z = x ++: y
    *      z: scala.collection.immutable.Vector[Int] = Vector(1, 2)
    *    }}}
    *
    *    @return       a new $coll which contains all elements of `prefix` followed
    *                  by all the elements of this $coll.
    */
  def prependAll[B >: A](prefix: Iterable[B]): CC[B] = fromIterable(View.Concat(prefix, toIterable))

  /** Alias for `prependAll` */
  @`inline` final def ++: [B >: A](prefix: Iterable[B]): CC[B] = prependAll(prefix)

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    *  @param   xs  The iterable providing the second half of each result pair
    *  @tparam  B     the type of the second half of the returned pairs
    *  @return        a new collection of type `That` containing pairs consisting of
    *                 corresponding elements of this $coll and `that`. The length
    *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B](xs: Iterable[B]): CC[(A @uncheckedVariance, B)] = fromIterable(View.Zip(toIterable, xs))
  // sound bcs of VarianceNote

  /** Zips this $coll with its indices.
    *
    *  @return        A new collection of type `That` containing pairs consisting of all elements of this
    *                 $coll paired with their index. Indices start at `0`.
    *  @example
    *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
    */
  def zipWithIndex: CC[(A @uncheckedVariance, Int)] = fromIterable(View.ZipWithIndex(toIterable))

  /** Converts this $coll of pairs into two collections of the first and second
    *  half of each pair.
    *
    *    {{{
    *    val xs = $Coll(
    *               (1, "one"),
    *               (2, "two"),
    *               (3, "three")).unzip
    *    // xs == ($Coll(1, 2, 3),
    *    //        $Coll(one, two, three))
    *    }}}
    *
    *  @tparam A1    the type of the first half of the element pairs
    *  @tparam A2    the type of the second half of the element pairs
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this $coll is a pair.
    *  @return       a pair of ${coll}s, containing the first, respectively second
    *                half of each element pair of this $coll.
    */
  def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (CC[A1], CC[A2]) = {
    val unzipped = View.Unzip(toIterable)
    (fromIterable(unzipped.first), fromIterable(unzipped.second))
  }

}

object Iterable extends IterableFactory.Delegate[Iterable](immutable.Iterable)
