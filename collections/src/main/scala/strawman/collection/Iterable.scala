package strawman
package collection

import scala.annotation.unchecked.uncheckedVariance
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.{Any, AnyRef, Array, Boolean, Either, `inline`, Int, None, Numeric, Option, Ordering, PartialFunction, StringContext, Some, Unit, deprecated, IllegalArgumentException, Function1}
import java.lang.{String, UnsupportedOperationException}
import scala.Predef.<:<

import strawman.collection.mutable.{ArrayBuffer, Builder, StringBuilder}
import java.lang.String

/** Base trait for generic collections.
  *
  * @tparam A the element type of the collection
  *
  * @define Coll `Iterable`
  * @define coll iterable collection
  */
trait Iterable[+A] extends IterableOnce[A] with IterableOps[A, Iterable, Iterable[A]] with Traversable[A] {

  // The collection itself
  final def toIterable: this.type = this

  //TODO scalac generates an override for this in AbstractMap; Making it final leads to a VerifyError
  protected[this] def coll: this.type = this

}

/** Base trait for Iterable operations
  *
  * =VarianceNote=
  *
  * We require that for all child classes of Iterable the variance of
  * the child class and the variance of the `C` parameter passed to `IterableOps`
  * are the same. We cannot express this since we lack variance polymorphism. That's
  * why we have to resort at some places to write `C[A @uncheckedVariance]`.
  *
  * @tparam CC type constructor of the collection (e.g. `List`, `Set`). Operations returning a collection
  *            with a different type of element `B` (e.g. `map`) return a `CC[B]`.
  * @tparam C  type of the collection (e.g. `List[Int]`, `String`, `BitSet`). Operations returning a collection
  *            with the same type of element (e.g. `drop`, `filter`) return a `C`.
  *
  * @define Coll Iterable
  * @define coll iterable collection
  * @define orderDependent
  *
  *    Note: might return different results for different runs, unless the underlying collection type is ordered.
  * @define orderDependentFold
  *
  *    Note: might return different results for different runs, unless the
  *    underlying collection type is ordered or the operator is associative
  *    and commutative.
  * @define mayNotTerminateInf
  *
  *    Note: may not terminate for infinite-sized collections.
  * @define willNotTerminateInf
  *
  *    Note: will not terminate for infinite-sized collections.
  *  @define undefinedorder
  *  The order in which operations are performed on elements is unspecified
  *  and may be nondeterministic.
  */
trait IterableOps[+A, +CC[_], +C] extends Any with IterableOnce[A] {

  /**
    * @return This collection as an `Iterable[A]`. No new collection will be built if `this` is already an `Iterable[A]`.
    */
  def toIterable: Iterable[A]

  /**
    * @return This collection as a `C`.
    */
  protected[this] def coll: C

  /**
    * Defines how to turn a given `Iterable[A]` into a collection of type `C`.
    *
    * This process can be done in a strict way or a non-strict way (ie. without evaluating
    * the elements of the resulting collections). In other words, this methods defines
    * the evaluation model of the collection.
    */
  protected[this] def fromSpecificIterable(coll: Iterable[A]): C

  /** Similar to `fromSpecificIterable`, but for a (possibly) different type of element.
    * Note that the return type is know `CC[E]`.
    */
  @`inline` final protected[this] def fromIterable[E](it: Iterable[E]): CC[E] = iterableFactory.from(it)

  /**
    * @return The companion object of this ${coll}, providing various factory methods.
    */
  def iterableFactory: IterableFactoryLike[CC]

  /**
    * @return a strict builder for the same collection type.
    *
    * Note that in the case of lazy collections (e.g. [[View]] or [[immutable.LazyList]]),
    * it is possible to implement this method but the resulting `Builder` will break laziness.
    * As a consequence, operations should preferably be implemented with `fromSpecificIterable`
    * instead of this method.
    */
  protected[this] def newSpecificBuilder(): Builder[A, C]

  // Consumes all the collection!
  protected[this] def reversed: Iterable[A] = {
    var xs: immutable.List[A] = immutable.Nil
    val it = iterator()
    while (it.hasNext) xs = it.next() :: xs
    xs
  }

  /** Apply `f` to each element for its side effects
   *  Note: [U] parameter needed to help scalac's type inference.
   */
  def foreach[U](f: A => U): Unit = iterator().foreach(f)

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if this $coll is empty or the given predicate `p`
   *                 holds for all elements of this $coll, otherwise `false`.
   */
  def forall(p: A => Boolean): Boolean = iterator().forall(p)

  /** Tests whether a predicate holds for at least one element of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` is satisfied by at least one element of this $coll, otherwise `false`
   */
  def exists(p: A => Boolean): Boolean = iterator().exists(p)

  /** Counts the number of elements in the $coll which satisfy a predicate.
   *
   *  @param p     the predicate  used to test elements.
   *  @return      the number of elements satisfying the predicate `p`.
   */
  def count(p: A => Boolean): Int = iterator().count(p)

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  $mayNotTerminateInf
    *  $orderDependent
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(p: A => Boolean): Option[A] = iterator().find(p)

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
  def foldLeft[B](z: B)(op: (B, A) => B): B = iterator().foldLeft(z)(op)

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
  def foldRight[B](z: B)(op: (A, B) => B): B = iterator().foldRight(z)(op)

  @deprecated("Use foldLeft instead of /:", "2.13.0")
  @`inline` final def /: [B](z: B)(op: (B, A) => B): B = foldLeft[B](z)(op)

  @deprecated("Use foldRight instead of :\\", "2.13.0")
  @`inline` final def :\ [B](z: B)(op: (A, B) => B): B = foldRight[B](z)(op)

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
  def reduce[B >: A](op: (B, B) => B): B = iterator().reduce(op)

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
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = iterator().reduceOption(op)

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
  def reduceLeft[B >: A](op: (B, A) => B): B = iterator().reduceLeft(op)

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
  def reduceRight[B >: A](op: (A, B) => B): B = iterator().reduceRight(op)

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  an option value containing the result of `reduceLeft(op)` if this $coll is nonempty,
   *           `None` otherwise.
   */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = iterator().reduceLeftOption(op)

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
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = iterator().reduceRightOption(op)

  /** Tests whether the $coll is empty.
    *
    *  Note: Implementations in subclasses that are not repeatedly traversable must take
    *  care not to consume any elements when `isEmpty` is called.
    *
    *  @return    `true` if the $coll contains no elements, `false` otherwise.
    */
  def isEmpty: Boolean = !iterator().hasNext

  /** Tests whether the $coll is not empty.
    *
    *  @return    `true` if the $coll contains at least one element, `false` otherwise.
    */
  def nonEmpty: Boolean = iterator().hasNext

  /** Selects the first element of this $coll.
    *  $orderDependent
    *  @return  the first element of this $coll.
    *  @throws NoSuchElementException if the $coll is empty.
    */
  def head: A = iterator().next()

  /** Optionally selects the first element.
    *  $orderDependent
    *  @return  the first element of this $coll if it is nonempty,
    *           `None` if it is empty.
    */
  def headOption: Option[A] = {
    val it = iterator()
    if(it.hasNext) Some(it.next()) else None
  }

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    val it = iterator()
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

  @deprecated("Use .knownSize >=0 instead of .hasDefiniteSize", "2.13.0")
  @`inline` final def hasDefiniteSize = knownSize >= 0

  /** The size of this $coll.
    *
    *  $willNotTerminateInf
    *
    *  @return    the number of elements in this $coll.
    */
  def size: Int = if (knownSize >= 0) knownSize else iterator().length

  /** A view over the elements of this collection. */
  def view: View[A] = View.fromIteratorProvider(() => iterator())

  /** A view over a slice of the elements of this collection. */
  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  @`inline` final def view(from: Int, until: Int): View[A] = view.slice(from, until)

  /** Given a collection factory `factory`, convert this collection to the appropriate
    * representation for the current element type `A`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    *      xs.to(BitSet) // for xs: Iterable[Int]
    */
  def to[C1](factory: Factory[A, C1]): C1 = factory.fromSpecific(this)

  def toList: immutable.List[A] = immutable.List.from(this)

  def toVector: immutable.Vector[A] = immutable.Vector.from(this)

  def toMap[K, V](implicit ev: A <:< (K, V)): immutable.Map[K, V] =
    immutable.Map.from(this.asInstanceOf[IterableOnce[(K, V)]])

  def toSet[B >: A]: immutable.Set[B] = immutable.Set.from(this)

  def toSeq: immutable.Seq[A] = immutable.Seq.from(this)

  def toIndexedSeq: immutable.IndexedSeq[A] = immutable.IndexedSeq.from(this)

  @deprecated("Use LazyList.from(it) instead of it.toStream", "2.13.0")
  @`inline` final def toStream: immutable.LazyList[A] = immutable.LazyList.from(this)

  @deprecated("Use ArrayBuffer.from(it) instead of it.toBuffer", "2.13.0")
  @`inline` final def toBuffer[B >: A]: mutable.Buffer[B] = mutable.ArrayBuffer.from(this)

  @deprecated("Use .iterator() instead of .toIterator", "2.13.0")
  @`inline` final def toIterator: Iterator[A] = iterator()

  /** Convert collection to array. */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
    else ArrayBuffer.from(this).toArray[B]

  /** Copy all elements of this collection to array `xs`, starting at `start`. */
  def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
    var i = start
    val it = iterator()
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
    *
    *  @return  a string representation which starts the result of `toString`
    *           applied to this $coll. By default the string prefix is the
    *           simple name of the collection class $coll.
    */
  def className: String = {
    var string = toIterable.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }

  @deprecated("Use className instead of stringPrefix", "2.13.0")
  @`inline` final def stringPrefix: String = className

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

  /** Displays all elements of this $coll in a string using a separator string.
    *
    *  @param sep   the separator string.
    *  @return      a string representation of this $coll. In the resulting string
    *               the string representations (w.r.t. the method `toString`)
    *               of all elements of this $coll are separated by the string `sep`.
    *
    *  @example  `List(1, 2, 3).mkString("|") = "1|2|3"`
    */
  def mkString(sep: String): String = mkString("", sep, "")

  /** Displays all elements of this $coll in a string.
    *
    *  @return a string representation of this $coll. In the resulting string
    *          the string representations (w.r.t. the method `toString`)
    *          of all elements of this $coll follow each other without any
    *          separator string.
    */
  def mkString: String = mkString("")

  override def toString = mkString(className + "(", ", ", ")")

  //TODO Can there be a useful lazy implementation of this method? Otherwise mark it as being always strict
  /** Transposes this $coll of iterable collections into
    *  a $coll of ${coll}s.
    *
    *    The resulting collection's type will be guided by the
    *    static type of $coll. For example:
    *
    *    {{{
    *    val xs = List(
    *               Set(1, 2, 3),
    *               Set(4, 5, 6)).transpose
    *    // xs == List(
    *    //         List(1, 4),
    *    //         List(2, 5),
    *    //         List(3, 6))
    *
    *    val ys = Vector(
    *               List(1, 2, 3),
    *               List(4, 5, 6)).transpose
    *    // ys == Vector(
    *    //         Vector(1, 4),
    *    //         Vector(2, 5),
    *    //         Vector(3, 6))
    *    }}}
    *
    *  @tparam B the type of the elements of each iterable collection.
    *  @param  asIterable an implicit conversion which asserts that the
    *          element type of this $coll is an `Iterable`.
    *  @return a two-dimensional $coll of ${coll}s which has as ''n''th row
    *          the ''n''th column of this $coll.
    *  @throws IllegalArgumentException if all collections in this $coll
    *          are not of the same size.
    */
  def transpose[B](implicit asIterable: A => /*<:<!!!*/ Iterable[B]): CC[CC[B] @uncheckedVariance] = {
    if (isEmpty)
      return iterableFactory.empty[CC[B]]

    def fail = throw new IllegalArgumentException("transpose requires all collections have the same size")

    val headSize = asIterable(head).size
    val bs: strawman.collection.immutable.IndexedSeq[Builder[B, CC[B]]] = strawman.collection.immutable.IndexedSeq.fill(headSize)(iterableFactory.newBuilder[B]())
    for (xs <- iterator()) {
      var i = 0
      for (x <- asIterable(xs)) {
        if (i >= headSize) fail
        bs(i) += x
        i += 1
      }
      if (i != headSize)
        fail
    }
    fromIterable(bs.map(_.result()))
  }

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
  def sum[B >: A](implicit num: Numeric[B]): B = iterator().sum[B]

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
  def product[B >: A](implicit num: Numeric[B]): B = iterator().product[B]

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
  def min[B >: A](implicit ord: Ordering[B]): A = iterator().min[B]

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
  def max[B >: A](implicit ord: Ordering[B]): A = iterator().max[B]

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
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = iterator().maxBy(f)

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
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = iterator().minBy(f)

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
    * of trait `Iterable`.
    *
    * @define coll iterable collection
    */
  class WithFilter(p: A => Boolean) extends collection.WithFilter[A, CC] {

    protected[this] def filtered = View.Filter(toIterable, p, isFlipped = false)

    def map[B](f: A => B): CC[B] = iterableFactory.from(View.Map(filtered, f))

    def flatMap[B](f: A => IterableOnce[B]): CC[B] = iterableFactory.from(View.FlatMap(filtered, f))

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
    val lead = iterator() drop n
    val it = iterator()
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
    val lead = iterator() drop n
    val it = iterator()
    while (lead.hasNext) {
      b += it.next()
      lead.next()
    }
    b.result()
  }

  /** Drops longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest suffix of this $coll whose first element
    *           does not satisfy the predicate `p`.
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
    iterator().grouped(size).map(fromSpecificIterable)

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
    iterator().sliding(size, step).map(fromSpecificIterable)

  /** The rest of the collection without its first element. */
  def tail: C = {
    if (isEmpty) throw new UnsupportedOperationException
    drop(1)
  }

  /** The initial part of the collection without its last element. */
  def init: C = {
    if (isEmpty) throw new UnsupportedOperationException
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
    val it = iterator()
    while (it.hasNext) {
      val elem = it.next()
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, newSpecificBuilder())
      bldr += elem
    }
    var result = immutable.HashMap.empty[K, C]
    val mapIt = m.iterator()
    while (mapIt.hasNext) {
      val (k, v) = mapIt.next()
      result = result.updated(k, v.result())
    }
    result
  }

  /**
    * Partitions this $coll into a map of ${coll}s according to a discriminator function `key`.
    * Each element in a group is transformed into a value of type `B` using the `value` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f))`, but more efficient.
    *
    * {{{
    *   case class User(name: String, age: Int)
    *
    *   def namesByAge(users: Seq[User]): Map[Int, Seq[String]] =
    *     users.groupMap(_.age)(_.name)
    * }}}
    *
    * @param key the discriminator function
    * @param f the element transformation function
    * @tparam K the type of keys returned by the discriminator function
    * @tparam B the type of values returned by the transformation function
    */
  def groupMap[K, B](key: A => K)(f: A => B): immutable.Map[K, CC[B]] = {
    val m = mutable.Map.empty[K, Builder[B, CC[B]]]
    for (elem <- this) {
      val k = key(elem)
      val bldr = m.getOrElseUpdate(k, iterableFactory.newBuilder[B]())
      bldr += f(elem)
    }
    var result = immutable.Map.empty[K, CC[B]]
    m.foreach { case (k, v) =>
      result = result + ((k, v.result()))
    }
    result
  }

  /**
    * Partitions this $coll into a map according to a discriminator function `key`. All the values that
    * have the same discriminator are then transformed by the `value` function and then reduced into a
    * single value with the `reduce` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f).reduce(reduce))`, but more efficient.
    *
    * {{{
    *   def occurrences[A](as: Seq[A]): Map[A, Int] =
    *     as.groupMapReduce(identity)(_ => 1)(_ + _)
    * }}}
    */
  def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): immutable.Map[K, B] = {
    val m = mutable.Map.empty[K, B]
    for (elem <- this) {
      val k = key(elem)
      val v =
        m.get(k) match {
          case Some(b) => reduce(b, f(elem))
          case None => f(elem)
        }
      m.put(k, v)
    }
    m.to(immutable.Map)
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

  /** Builds a new collection by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
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
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[B](f: A => IterableOnce[B]): CC[B] = fromIterable(View.FlatMap(toIterable, f))

  /** Converts this $coll of traversable collections into
    *  a $coll formed by the elements of these traversable
    *  collections.
    *
    *    The resulting collection's type will be guided by the
    *    type of $coll. For example:
    *
    *    {{{
    *    val xs = List(
    *               Set(1, 2, 3),
    *               Set(1, 2, 3)
    *             ).flatten
    *    // xs == List(1, 2, 3, 1, 2, 3)
    *
    *    val ys = Set(
    *               List(1, 2, 3),
    *               List(3, 2, 1)
    *             ).flatten
    *    // ys == Set(1, 2, 3)
    *    }}}
    *
    *  @tparam B the type of the elements of each traversable collection.
    *  @param asIterable an implicit conversion which asserts that the element
    *          type of this $coll is a `GenTraversable`.
    *  @return a new $coll resulting from concatenating all element ${coll}s.
    *
    */
  def flatten[B](implicit asIterable: A => IterableOnce[B]): CC[B] =
    fromIterable(View.FlatMap(toIterable, asIterable))

  /** Builds a new collection by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
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
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] =
    iterator().collectFirst(pf)

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix   the traversable to append.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def concat[B >: A](suffix: Iterable[B]): CC[B] = fromIterable(View.Concat(toIterable, suffix))

  /** Alias for `concat` */
  @`inline` final def ++ [B >: A](suffix: Iterable[B]): CC[B] = concat(suffix)

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    *  @param   that  The iterable providing the second half of each result pair
    *  @tparam  B     the type of the second half of the returned pairs
    *  @return        a new $coll containing pairs consisting of corresponding elements of this $coll and `that`.
    *                 The length of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B](that: Iterable[B]): CC[(A @uncheckedVariance, B)] = fromIterable(View.Zip(toIterable, that))
  // sound bcs of VarianceNote

  /** Zips this $coll with its indices.
    *
    *  @return        A new $coll containing pairs consisting of all elements of this $coll paired with their index.
    *                 Indices start at `0`.
    *  @example
    *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
    */
  def zipWithIndex: CC[(A @uncheckedVariance, Int)] = fromIterable(View.ZipWithIndex(toIterable))

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is shorter than the other,
    *  placeholder elements are used to extend the shorter collection to the length of the longer.
    *
    *  @param that     the iterable providing the second half of each result pair
    *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    *  @return        a new collection of type `That` containing pairs consisting of
    *                 corresponding elements of this $coll and `that`. The length
    *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
    *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    */
  def zipAll[A1 >: A, B](that: Iterable[B], thisElem: A1, thatElem: B): CC[(A1, B)] = fromIterable(View.ZipAll(toIterable, that, thisElem, thatElem))

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
  def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
    val unzipped = View.Unzip(toIterable)
    (fromIterable(unzipped.first), fromIterable(unzipped.second))
  }

  /** Iterates over the tails of this $coll. The first value will be this
    *  $coll and the final one will be an empty $coll, with the intervening
    *  values the results of successive applications of `tail`.
    *
    *  @return   an iterator over all the tails of this $coll
    *  @example  `List(1,2,3).tails = Iterator(List(1,2,3), List(2,3), List(3), Nil)`
    */
  def tails: Iterator[C] = iterateUntilEmpty(_.tail)

  /** Iterates over the inits of this $coll. The first value will be this
    *  $coll and the final one will be an empty $coll, with the intervening
    *  values the results of successive applications of `init`.
    *
    *  @return  an iterator over all the inits of this $coll
    *  @example  `List(1,2,3).inits = Iterator(List(1,2,3), List(1,2), List(1), Nil)`
    */
  def inits: Iterator[C] = iterateUntilEmpty(_.init)

  // A helper for tails and inits.
  private[this] def iterateUntilEmpty(f: Iterable[A] => Iterable[A]): Iterator[C] = {
    val it = Iterator.iterate(toIterable)(f).takeWhile(x => !x.isEmpty)
    (it ++ Iterator(Iterable.empty)).map(fromSpecificIterable)
  }
}

object Iterable extends IterableFactory.Delegate[Iterable](immutable.Iterable) {
  implicit def toLazyZipOps[A, CC[X] <: Iterable[X]](that: CC[A]): LazyZipOps[A, CC[A]] = new LazyZipOps(that)
}

abstract class AbstractIterable[+A] extends Iterable[A]