/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package math

import java.util.Comparator

import scala.language.implicitConversions
import scala.annotation.migration

/** Ordering is a trait whose instances each represent a strategy for sorting
  * instances of a type.
  *
  * Ordering's companion object defines many implicit objects to deal with
  * subtypes of [[AnyVal]] (e.g. `Int`, `Double`), `String`, and others.
  *
  * To sort instances by one or more member variables, you can take advantage
  * of these built-in orderings using [[Ordering.by]] and [[Ordering.on]]:
  *
  * {{{
  * import scala.util.Sorting
  * val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))
  *
  * // sort by 2nd element
  * Sorting.quickSort(pairs)(Ordering.by[(String, Int, Int), Int](_._2))
  *
  * // sort by the 3rd element, then 1st
  * Sorting.quickSort(pairs)(Ordering[(Int, String)].on(x => (x._3, x._1)))
  * }}}
  *
  * An `Ordering[T]` is implemented by specifying the [[compare]] method,
  * `compare(a: T, b: T): Int`, which decides how to order two instances
  * `a` and `b`. Instances of `Ordering[T]` can be used by things like
  * `scala.util.Sorting` to sort collections like `Array[T]`.
  *
  * For example:
  *
  * {{{
  * import scala.util.Sorting
  *
  * case class Person(name:String, age:Int)
  * val people = Array(Person("bob", 30), Person("ann", 32), Person("carl", 19))
  *
  * // sort by age
  * object AgeOrdering extends Ordering[Person] {
  *   def compare(a:Person, b:Person) = a.age.compare(b.age)
  * }
  * Sorting.quickSort(people)(AgeOrdering)
  * }}}
  *
  * This trait and [[scala.math.Ordered]] both provide this same functionality, but
  * in different ways. A type `T` can be given a single way to order itself by
  * extending `Ordered`. Using `Ordering`, this same type may be sorted in many
  * other ways. `Ordered` and `Ordering` both provide implicits allowing them to be
  * used interchangeably.
  *
  * You can `import scala.math.Ordering.Implicits._` to gain access to other
  * implicit orderings.
  *
  * @see [[scala.math.Ordered]], [[scala.util.Sorting]], [[scala.math.Ordering.Implicits]]
  */
@annotation.implicitNotFound(msg = "No implicit Ordering defined for ${T}.")
trait Ordering[T] extends Comparator[T] with PartialOrdering[T] with Serializable {
  outer =>

  /** Returns whether a comparison between `x` and `y` is defined, and if so
    * the result of `compare(x, y)`.
    */
  def tryCompare(x: T, y: T) = Some(compare(x, y))

 /** Returns an integer whose sign communicates how x compares to y.
   *
   * The result sign has the following meaning:
   *
   *  - negative if x < y
   *  - positive if x > y
   *  - zero otherwise (if x == y)
   */
  def compare(x: T, y: T): Int

  /** Return true if `x` <= `y` in the ordering. */
  override def lteq(x: T, y: T): Boolean = compare(x, y) <= 0

  /** Return true if `x` >= `y` in the ordering. */
  override def gteq(x: T, y: T): Boolean = compare(x, y) >= 0

  /** Return true if `x` < `y` in the ordering. */
  override def lt(x: T, y: T): Boolean = compare(x, y) < 0

  /** Return true if `x` > `y` in the ordering. */
  override def gt(x: T, y: T): Boolean = compare(x, y) > 0

  /** Return true if `x` == `y` in the ordering. */
  override def equiv(x: T, y: T): Boolean = compare(x, y) == 0

  /** Return `x` if `x` >= `y`, otherwise `y`. */
  def max[U <: T](x: U, y: U): U = if (gteq(x, y)) x else y

  /** Return `x` if `x` <= `y`, otherwise `y`. */
  def min[U <: T](x: U, y: U): U = if (lteq(x, y)) x else y

  /** Return the opposite ordering of this one.
    *
    * Implementations overriding this method MUST override [[isReverseOf]]
    * as well if they change the behavior at all (for example, caching does
    * not require overriding it).
    */
  override def reverse: Ordering[T] = new Ordering.Reverse[T](this)

  /** Returns whether or not the other ordering is the opposite
    * ordering of this one.
    *
    * Equivalent to `other == this.reverse`.
    *
    * Implementations should only override this method if they are overriding
    * [[reverse]] as well.
    */
  def isReverseOf(other: Ordering[_]): Boolean = other match {
    case that: Ordering.Reverse[_] => that.outer == this
    case _ => false
  }

  /** Given f, a function from U into T, creates an Ordering[U] whose compare
    * function is equivalent to:
    *
    * {{{
    * def compare(x:U, y:U) = Ordering[T].compare(f(x), f(y))
    * }}}
    */
  def on[U](f: U => T): Ordering[U] = new Ordering[U] {
    def compare(x: U, y: U) = outer.compare(f(x), f(y))
  }

  /** Creates an Ordering[T] whose compare function returns the
    * result of this Ordering's compare function, if it is non-zero,
    * or else the result of `other`s compare function.
    *
    * @example
    * {{{
    * case class Pair(a: Int, b: Int)
    *
    * val pairOrdering = Ordering.by[Pair, Int](_.a)
    *                            .orElse(Ordering.by[Pair, Int](_.b))
    * }}}
    *
    * @param other an Ordering to use if this Ordering returns zero
    */
  def orElse(other: Ordering[T]): Ordering[T] = (x, y) => {
    val res1 = outer.compare(x, y)
    if (res1 != 0) res1 else other.compare(x, y)
  }

  /** Given f, a function from T into S, creates an Ordering[T] whose compare
    * function returns the result of this Ordering's compare function,
    * if it is non-zero, or else a result equivalent to:
    *
    * {{{
    * Ordering[S].compare(f(x), f(y))
    * }}}
    *
    * This function is equivalent to passing the result of `Ordering.by(f)`
    * to `orElse`.
    *
    * @example
    * {{{
    * case class Pair(a: Int, b: Int)
    *
    * val pairOrdering = Ordering.by[Pair, Int](_.a)
    *                            .orElseBy[Int](_.b)
    * }}}
    */
  def orElseBy[S](f: T => S)(implicit ord: Ordering[S]): Ordering[T] = (x, y) => {
    val res1 = outer.compare(x, y)
    if (res1 != 0) res1 else ord.compare(f(x), f(y))
  }

  /** This inner class defines comparison operators available for `T`.
   *
   * It can't extend `AnyVal` because it is not a top-level class
   * or a member of a statically accessible object.
   */
  class OrderingOps(lhs: T) {
    def <(rhs: T): Boolean = lt(lhs, rhs)
    def <=(rhs: T): Boolean = lteq(lhs, rhs)
    def >(rhs: T): Boolean = gt(lhs, rhs)
    def >=(rhs: T): Boolean = gteq(lhs, rhs)
    def equiv(rhs: T): Boolean = Ordering.this.equiv(lhs, rhs)
    def max(rhs: T): T = Ordering.this.max(lhs, rhs)
    def min(rhs: T): T = Ordering.this.min(lhs, rhs)
  }

  /** This implicit method augments `T` with the comparison operators defined
    * in `scala.math.Ordering.Ops`.
    */
  implicit def mkOrderingOps(lhs: T): OrderingOps = new OrderingOps(lhs)
}

trait LowPriorityOrderingImplicits {

  type AsComparable[A] = A => Comparable[_ >: A]

  /** This would conflict with all the nice implicit Orderings
   *  available, but thanks to the magic of prioritized implicits
   *  via subclassing we can make `Ordered[A] => Ordering[A]` only
   *  turn up if nothing else works.  Since `Ordered[A]` extends
   *  `Comparable[A]` anyway, we can throw in some Java interop too.
   */
  implicit def ordered[A](implicit asComparable: AsComparable[A]): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A): Int = asComparable(x).compareTo(y)
  }

  implicit def comparatorToOrdering[A](implicit cmp: Comparator[A]): Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A) = cmp.compare(x, y)
  }
}

/** This is the companion object for the [[scala.math.Ordering]] trait.
  *
  * It contains many implicit orderings as well as well as methods to construct
  * new orderings.
  */
object Ordering extends LowPriorityOrderingImplicits {
  private final val reverseSeed  = 41
  private final val optionSeed   = 43
  private final val iterableSeed = 47

  @inline def apply[T](implicit ord: Ordering[T]) = ord

  /** An ordering which caches the value of its reverse. */
  sealed trait CachedReverse[T] extends Ordering[T] {
    private[this] val _reverse = super.reverse
    override final def reverse: Ordering[T] = _reverse
    override final def isReverseOf(other: Ordering[_]): Boolean = other eq _reverse
  }

  /** A reverse ordering */
  private final class Reverse[T](private[Ordering] val outer: Ordering[T]) extends Ordering[T] {
    override def reverse: Ordering[T]                   = outer
    override def isReverseOf(other: Ordering[_]): Boolean = other == outer

    def compare(x: T, y: T): Int            = outer.compare(y, x)
    override def lteq(x: T, y: T): Boolean  = outer.lteq(y, x)
    override def gteq(x: T, y: T): Boolean  = outer.gteq(y, x)
    override def lt(x: T, y: T): Boolean    = outer.lt(y, x)
    override def gt(x: T, y: T): Boolean    = outer.gt(y, x)
    override def equiv(x: T, y: T): Boolean = outer.equiv(y, x)
    override def max[U <: T](x: U, y: U): U = outer.min(x, y)
    override def min[U <: T](x: U, y: U): U = outer.max(x, y)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Reverse[_]             => this.outer == that.outer
      case _                            => false
    }
    override def hashCode(): Int = outer.hashCode() * reverseSeed
  }

  @SerialVersionUID(-2996748994664583574L)
  private final class IterableOrdering[CC[X] <: Iterable[X], T](private val ord: Ordering[T]) extends Ordering[CC[T]] {
    def compare(x: CC[T], y: CC[T]): Int = {
      val xe = x.iterator
      val ye = y.iterator

      while (xe.hasNext && ye.hasNext) {
        val res = ord.compare(xe.next(), ye.next())
        if (res != 0) return res
      }

      Boolean.compare(xe.hasNext, ye.hasNext)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that  => true
      case that: IterableOrdering[_, _]  => this.ord == that.ord
      case _                             => false
    }
    override def hashCode(): Int = ord.hashCode() * iterableSeed
  }

  trait ExtraImplicits {
    /** Not in the standard scope due to the potential for divergence:
      * For instance `implicitly[Ordering[Any]]` diverges in its presence.
      */
    implicit def seqOrdering[CC[X] <: scala.collection.Seq[X], T](implicit ord: Ordering[T]): Ordering[CC[T]] =
      new IterableOrdering[CC, T](ord)

    implicit def sortedSetOrdering[CC[X] <: scala.collection.SortedSet[X], T](implicit ord: Ordering[T]): Ordering[CC[T]] =
      new IterableOrdering[CC, T](ord)

    /** This implicit creates a conversion from any value for which an
      * implicit `Ordering` exists to the class which creates infix operations.
      * With it imported, you can write methods as follows:
      *
      * {{{
      * def lessThan[T: Ordering](x: T, y: T) = x < y
      * }}}
      */
    implicit def infixOrderingOps[T](x: T)(implicit ord: Ordering[T]): Ordering[T]#OrderingOps = new ord.OrderingOps(x)
  }

  /** An object containing implicits which are not in the default scope. */
  object Implicits extends ExtraImplicits { }

  /** Construct an Ordering[T] given a function `lt`. */
  def fromLessThan[T](cmp: (T, T) => Boolean): Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = if (cmp(x, y)) -1 else if (cmp(y, x)) 1 else 0
    // overrides to avoid multiple comparisons
    override def lt(x: T, y: T): Boolean = cmp(x, y)
    override def gt(x: T, y: T): Boolean = cmp(y, x)
    override def gteq(x: T, y: T): Boolean = !cmp(x, y)
    override def lteq(x: T, y: T): Boolean = !cmp(y, x)
  }

  /** Given f, a function from T into S, creates an Ordering[T] whose compare
    * function is equivalent to:
    *
    * {{{
    * def compare(x:T, y:T) = Ordering[S].compare(f(x), f(y))
    * }}}
    *
    * This function is an analogue to Ordering.on where the Ordering[S]
    * parameter is passed implicitly.
    */
  def by[T, S](f: T => S)(implicit ord: Ordering[S]): Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = ord.compare(f(x), f(y))
    override def lt(x: T, y: T): Boolean = ord.lt(f(x), f(y))
    override def gt(x: T, y: T): Boolean = ord.gt(f(x), f(y))
    override def gteq(x: T, y: T): Boolean = ord.gteq(f(x), f(y))
    override def lteq(x: T, y: T): Boolean = ord.lteq(f(x), f(y))
  }

  trait UnitOrdering extends Ordering[Unit] {
    def compare(x: Unit, y: Unit) = 0
  }
  @SerialVersionUID(4089257611611206746L)
  implicit object Unit extends UnitOrdering

  trait BooleanOrdering extends Ordering[Boolean] {
    def compare(x: Boolean, y: Boolean): Int = java.lang.Boolean.compare(x, y)
  }
  @SerialVersionUID(-94703182178890445L)
  implicit object Boolean extends BooleanOrdering

  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte): Int = java.lang.Byte.compare(x, y)
  }
  @SerialVersionUID(-2268545360148786406L)
  implicit object Byte extends ByteOrdering

  trait CharOrdering extends Ordering[Char] {
    def compare(x: Char, y: Char): Int = java.lang.Character.compare(x, y)
  }
  @SerialVersionUID(2588141633104296698L)
  implicit object Char extends CharOrdering

  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short): Int = java.lang.Short.compare(x, y)
  }
  @SerialVersionUID(4919657051864630912L)
  implicit object Short extends ShortOrdering

  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int): Int = java.lang.Integer.compare(x, y)
  }
  @SerialVersionUID(-8412871093094815037L)
  implicit object Int extends IntOrdering with CachedReverse[Int]

  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long): Int = java.lang.Long.compare(x, y)
  }
  @SerialVersionUID(-5231423581640563981L)
  implicit object Long extends LongOrdering

  /** `Ordering`s for `Float`s.
    *
    * The behavior of the comparison operations provided by the default (implicit)
    * ordering on `Float` changed in 2.10.0 and 2.13.0.
    * Prior to Scala 2.10.0, the `Ordering` instance used semantics
    * consistent with `java.lang.Float.compare`.
    *
    * Scala 2.10.0 changed the implementation of `lt`, `equiv`, `min`, etc., to be
    * IEEE 754 compliant, while keeping the `compare` method NOT compliant,
    * creating an internally inconsistent instance. IEEE 754 specifies that
    * `0.0F == -0.0F`. In addition, it requires all comparisons with `Float.NaN` return
    * `false` thus `0.0F < Float.NaN`, `0.0F > Float.NaN`, and
    * `Float.NaN == Float.NaN` all yield `false`, analogous `None` in `flatMap`.
    *
    * Recognizing the limitation of the IEEE 754 semantics in terms of ordering,
    * Scala 2.13.0 created two instances: `Ordering.Float.IeeeOrdering`, which retains
    * the IEEE 754 semantics from Scala 2.12.x, and `Ordering.Float.TotalOrdering`,
    * which brings back the `java.lang.Float.compare` semantics for all operations.
    * The default extends `TotalOrdering`.
    *
    *  {{{
    *  List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).sorted      // List(-Infinity, 0.0, 1.0, NaN)
    *  List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).min         // -Infinity
    *  implicitly[Ordering[Float]].lt(0.0F, 0.0F / 0.0F)       // true
    *  {
    *    import Ordering.Float.IeeeOrdering
    *    List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).sorted    // List(-Infinity, 0.0, 1.0, NaN)
    *    List(0.0F, 1.0F, 0.0F / 0.0F, -1.0F / 0.0F).min       // NaN
    *    implicitly[Ordering[Float]].lt(0.0F, 0.0F / 0.0F)     // false
    *  }
    *  }}}
    *
    * @define floatOrdering Because the behavior of `Float`s specified by IEEE is
    *                       not consistent with a total ordering when dealing with
    *                       `NaN`, there are two orderings defined for `Float`:
    *                       `TotalOrdering`, which is consistent with a total
    *                       ordering, and `IeeeOrdering`, which is consistent
    *                       as much as possible with IEEE spec and floating point
    *                       operations defined in [[scala.math]].
    */
  object Float {
    /** An ordering for `Float`s which is a fully consistent total ordering,
      * and treats `NaN` as larger than all other `Float` values; it behaves
      * the same as [[java.lang.Float.compare]].
      *
      * $floatOrdering
      *
      * This ordering may be preferable for sorting collections.
      *
      * @see [[IeeeOrdering]]
      */
    trait TotalOrdering extends Ordering[Float] {
      def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)
    }
    @SerialVersionUID(2951539161283192433L)
    implicit object TotalOrdering extends TotalOrdering

    /** An ordering for `Float`s which is consistent with IEEE specifications
      * whenever possible.
      *
      *   - `lt`, `lteq`, `equiv`, `gteq` and `gt` are consistent with primitive
      * comparison operations for `Float`s, and return `false` when called with
      * `NaN`.
      *   - `min` and `max` are consistent with `math.min` and `math.max`, and
      * return `NaN` when called with `NaN` as either argument.
      *   - `compare` behaves the same as [[java.lang.Float.compare]].
      *
      * $floatOrdering
      *
      * This ordering may be preferable for numeric contexts.
      *
      * @see [[TotalOrdering]]
      */
    trait IeeeOrdering extends Ordering[Float] {
      def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)

      override def lteq(x: Float, y: Float): Boolean = x <= y
      override def gteq(x: Float, y: Float): Boolean = x >= y
      override def lt(x: Float, y: Float): Boolean = x < y
      override def gt(x: Float, y: Float): Boolean = x > y
      override def equiv(x: Float, y: Float): Boolean = x == y
      override def max[U <: Float](x: U, y: U): U = math.max(x, y).asInstanceOf[U]
      override def min[U <: Float](x: U, y: U): U = math.min(x, y).asInstanceOf[U]
    }
    @SerialVersionUID(2142189527751553605L)
    implicit object IeeeOrdering extends IeeeOrdering
  }
  @migration(
    "  The default implicit ordering for floats now maintains consistency\n" +
    "  between its `compare` method and its `lt`, `min`, `equiv`, etc., methods,\n" +
    "  which means nonconforming to IEEE 754's behavior for -0.0F and NaN.\n" +
    "  The sort order of floats remains the same, however, with NaN at the end.\n" +
    "  Import Ordering.Float.IeeeOrdering to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Ordering$$Float$.html.", "2.13.0")
  @SerialVersionUID(-8500693657289762132L)
  implicit object DeprecatedFloatOrdering extends Float.TotalOrdering

  /** `Ordering`s for `Double`s.
    *
    * The behavior of the comparison operations provided by the default (implicit)
    * ordering on `Double` changed in 2.10.0 and 2.13.0.
    * Prior to Scala 2.10.0, the `Ordering` instance used semantics
    * consistent with `java.lang.Double.compare`.
    *
    * Scala 2.10.0 changed the implementation of `lt`, `equiv`, `min`, etc., to be
    * IEEE 754 compliant, while keeping the `compare` method NOT compliant,
    * creating an internally inconsistent instance. IEEE 754 specifies that
    * `0.0 == -0.0`. In addition, it requires all comparisons with `Double.NaN` return
    * `false` thus `0.0 < Double.NaN`, `0.0 > Double.NaN`, and
    * `Double.NaN == Double.NaN` all yield `false`, analogous `None` in `flatMap`.
    *
    * Recognizing the limitation of the IEEE 754 semantics in terms of ordering,
    * Scala 2.13.0 created two instances: `Ordering.Double.IeeeOrdering`, which retains
    * the IEEE 754 semantics from Scala 2.12.x, and `Ordering.Double.TotalOrdering`,
    * which brings back the `java.lang.Double.compare` semantics for all operations.
    * The default extends `TotalOrdering`.
    *
    *  {{{
    *  List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).sorted      // List(-Infinity, 0.0, 1.0, NaN)
    *  List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).min         // -Infinity
    *  implicitly[Ordering[Double]].lt(0.0, 0.0 / 0.0)   // true
    *  {
    *    import Ordering.Double.IeeeOrdering
    *    List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).sorted    // List(-Infinity, 0.0, 1.0, NaN)
    *    List(0.0, 1.0, 0.0 / 0.0, -1.0 / 0.0).min       // NaN
    *    implicitly[Ordering[Double]].lt(0.0, 0.0 / 0.0) // false
    *  }
    *  }}}
    *
    * @define doubleOrdering Because the behavior of `Double`s specified by IEEE is
    *                        not consistent with a total ordering when dealing with
    *                        `NaN`, there are two orderings defined for `Double`:
    *                        `TotalOrdering`, which is consistent with a total
    *                        ordering, and `IeeeOrdering`, which is consistent
    *                        as much as possible with IEEE spec and floating point
    *                        operations defined in [[scala.math]].
    */
  object Double {
    /** An ordering for `Double`s which is a fully consistent total ordering,
      * and treats `NaN` as larger than all other `Double` values; it behaves
      * the same as [[java.lang.Double.compare]].
      *
      * $doubleOrdering
      *
      * This ordering may be preferable for sorting collections.
      *
      * @see [[IeeeOrdering]]
      */
    trait TotalOrdering extends Ordering[Double] {
      def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)
    }
    @SerialVersionUID(-831119229746134011L)
    implicit object TotalOrdering extends TotalOrdering

    /** An ordering for `Double`s which is consistent with IEEE specifications
      * whenever possible.
      *
      *   - `lt`, `lteq`, `equiv`, `gteq` and `gt` are consistent with primitive
      * comparison operations for `Double`s, and return `false` when called with
      * `NaN`.
      *   - `min` and `max` are consistent with `math.min` and `math.max`, and
      * return `NaN` when called with `NaN` as either argument.
      *   - `compare` behaves the same as [[java.lang.Double.compare]].
      *
      * $doubleOrdering
      *
      * This ordering may be preferable for numeric contexts.
      *
      * @see [[TotalOrdering]]
      */
    trait IeeeOrdering extends Ordering[Double] {
      def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)

      override def lteq(x: Double, y: Double): Boolean = x <= y
      override def gteq(x: Double, y: Double): Boolean = x >= y
      override def lt(x: Double, y: Double): Boolean = x < y
      override def gt(x: Double, y: Double): Boolean = x > y
      override def equiv(x: Double, y: Double): Boolean = x == y
      override def max[U <: Double](x: U, y: U): U = math.max(x, y).asInstanceOf[U]
      override def min[U <: Double](x: U, y: U): U = math.min(x, y).asInstanceOf[U]
    }
    @SerialVersionUID(5722631152457877238L)
    implicit object IeeeOrdering extends IeeeOrdering
  }
  @migration(
    "  The default implicit ordering for doubles now maintains consistency\n" +
    "  between its `compare` method and its `lt`, `min`, `equiv`, etc., methods,\n" +
    "  which means nonconforming to IEEE 754's behavior for -0.0 and NaN.\n" +
    "  The sort order of doubles remains the same, however, with NaN at the end.\n" +
    "  Import Ordering.Double.IeeeOrdering to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Ordering$$Double$.html.", "2.13.0")
  @SerialVersionUID(-7340686892557971538L)
  implicit object DeprecatedDoubleOrdering extends Double.TotalOrdering

  trait BigIntOrdering extends Ordering[BigInt] {
    def compare(x: BigInt, y: BigInt) = x.compare(y)
  }
  @SerialVersionUID(-3075297647817530785L)
  implicit object BigInt extends BigIntOrdering

  trait BigDecimalOrdering extends Ordering[BigDecimal] {
    def compare(x: BigDecimal, y: BigDecimal) = x.compare(y)
  }
  @SerialVersionUID(-833457937756812905L)
  implicit object BigDecimal extends BigDecimalOrdering

  trait StringOrdering extends Ordering[String] {
    def compare(x: String, y: String) = x.compareTo(y)
  }
  @SerialVersionUID(1302240016074071079L)
  implicit object String extends StringOrdering

  trait SymbolOrdering extends Ordering[Symbol] {
    def compare(x: Symbol, y: Symbol): Int = x.name.compareTo(y.name)
  }
  @SerialVersionUID(1996702162912307637L)
  implicit object Symbol extends SymbolOrdering

  trait OptionOrdering[T] extends Ordering[Option[T]] {
    def optionOrdering: Ordering[T]
    def compare(x: Option[T], y: Option[T]) = (x, y) match {
      case (None, None)       => 0
      case (None, _)          => -1
      case (_, None)          => 1
      case (Some(x), Some(y)) => optionOrdering.compare(x, y)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: OptionOrdering[_]      => this.optionOrdering == that.optionOrdering
      case _                            => false
    }
    override def hashCode(): Int = optionOrdering.hashCode() * optionSeed
  }
  implicit def Option[T](implicit ord: Ordering[T]): Ordering[Option[T]] = {
    @SerialVersionUID(6958068162830323876L)
    class O extends  OptionOrdering[T] { val optionOrdering = ord }
    new O()
  }

  /** @deprecated Iterables are not guaranteed to have a consistent order, so the `Ordering`
    *             returned by this method may not be stable or meaningful. If you are using a type
    *             with a consistent order (such as `Seq`), use its `Ordering` (found in the
    *             [[Implicits]] object) instead.
    */
  @deprecated("Iterables are not guaranteed to have a consistent order; if using a type with a " +
    "consistent order (e.g. Seq), use its Ordering (found in the Ordering.Implicits object)", since = "2.13.0")
  implicit def Iterable[T](implicit ord: Ordering[T]): Ordering[Iterable[T]] =
    new IterableOrdering[Iterable, T](ord)

  implicit def Tuple2[T1, T2](implicit ord1: Ordering[T1], ord2: Ordering[T2]): Ordering[(T1, T2)] =
    new Tuple2Ordering(ord1, ord2)

  @SerialVersionUID(4945084135299531202L)
  private[this] final class Tuple2Ordering[T1, T2](private val ord1: Ordering[T1],
                                                   private val ord2: Ordering[T2]) extends Ordering[(T1, T2)] {
    def compare(x: (T1, T2), y: (T1, T2)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      ord2.compare(x._2, y._2)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple2Ordering[_, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2).hashCode()
  }

  implicit def Tuple3[T1, T2, T3](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3]) : Ordering[(T1, T2, T3)] =
    new Tuple3Ordering(ord1, ord2, ord3)

  @SerialVersionUID(-5367223704121832335L)
  private[this] final class Tuple3Ordering[T1, T2, T3](private val ord1: Ordering[T1],
                                                       private val ord2: Ordering[T2],
                                                       private val ord3: Ordering[T3]) extends Ordering[(T1, T2, T3)] {
    def compare(x: (T1, T2, T3), y: (T1, T2, T3)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      ord3.compare(x._3, y._3)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple3Ordering[_, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3).hashCode()
  }

  implicit def Tuple4[T1, T2, T3, T4](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4]) : Ordering[(T1, T2, T3, T4)] =
    new Tuple4Ordering(ord1, ord2, ord3, ord4)

  @SerialVersionUID(-6055313861145218178L)
  private[this] final class Tuple4Ordering[T1, T2, T3, T4](private val ord1: Ordering[T1],
                                                           private val ord2: Ordering[T2],
                                                           private val ord3: Ordering[T3],
                                                           private val ord4: Ordering[T4])
    extends Ordering[(T1, T2, T3, T4)] {
    def compare(x: (T1, T2, T3, T4), y: (T1, T2, T3, T4)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      ord4.compare(x._4, y._4)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple4Ordering[_, _, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3, ord4).hashCode()
  }

  implicit def Tuple5[T1, T2, T3, T4, T5](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5]): Ordering[(T1, T2, T3, T4, T5)] =
    new Tuple5Ordering(ord1, ord2, ord3, ord4, ord5)

  @SerialVersionUID(-5517329921227646061L)
  private[this] final class Tuple5Ordering[T1, T2, T3, T4, T5](private val ord1: Ordering[T1],
                                                               private val ord2: Ordering[T2],
                                                               private val ord3: Ordering[T3],
                                                               private val ord4: Ordering[T4],
                                                               private val ord5: Ordering[T5])
    extends Ordering[(T1, T2, T3, T4, T5)] {
    def compare(x: (T1, T2, T3, T4, T5), y: (T1, T2, T3, T4, T5)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      ord5.compare(x._5, y._5)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple5Ordering[_, _, _, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5).hashCode()
  }

  @SerialVersionUID(3045467524192969060L)
  implicit def Tuple6[T1, T2, T3, T4, T5, T6](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6]): Ordering[(T1, T2, T3, T4, T5, T6)] =
    new Tuple6Ordering(ord1, ord2, ord3, ord4, ord5, ord6)

  private[this] final class Tuple6Ordering[T1, T2, T3, T4, T5, T6](private val ord1: Ordering[T1],
                                                                   private val ord2: Ordering[T2],
                                                                   private val ord3: Ordering[T3],
                                                                   private val ord4: Ordering[T4],
                                                                   private val ord5: Ordering[T5],
                                                                   private val ord6: Ordering[T6])
    extends Ordering[(T1, T2, T3, T4, T5, T6)] {
    def compare(x: (T1, T2, T3, T4, T5, T6), y: (T1, T2, T3, T4, T5, T6)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      ord6.compare(x._6, y._6)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple6Ordering[_, _, _, _, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6).hashCode()
  }

  implicit def Tuple7[T1, T2, T3, T4, T5, T6, T7](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6], ord7: Ordering[T7]): Ordering[(T1, T2, T3, T4, T5, T6, T7)] =
    new Tuple7Ordering(ord1, ord2, ord3, ord4, ord5, ord6, ord7)

  @SerialVersionUID(1253188205893682451L)
  private[this] final class Tuple7Ordering[T1, T2, T3, T4, T5, T6, T7](private val ord1: Ordering[T1],
                                                                       private val ord2: Ordering[T2],
                                                                       private val ord3: Ordering[T3],
                                                                       private val ord4: Ordering[T4],
                                                                       private val ord5: Ordering[T5],
                                                                       private val ord6: Ordering[T6],
                                                                       private val ord7: Ordering[T7])
    extends Ordering[(T1, T2, T3, T4, T5, T6, T7)] {
    def compare(x: (T1, T2, T3, T4, T5, T6, T7), y: (T1, T2, T3, T4, T5, T6, T7)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      val compare6 = ord6.compare(x._6, y._6)
      if (compare6 != 0) return compare6
      ord7.compare(x._7, y._7)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple7Ordering[_, _, _, _, _, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6 &&
        this.ord7 == that.ord7
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6, ord7).hashCode()
  }

  @SerialVersionUID(4003095353309354068L)
  implicit def Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6], ord7: Ordering[T7], ord8: Ordering[T8]): Ordering[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    new Tuple8Ordering(ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8)

  private[this] final class Tuple8Ordering[T1, T2, T3, T4, T5, T6, T7, T8](private val ord1: Ordering[T1],
                                                                           private val ord2: Ordering[T2],
                                                                           private val ord3: Ordering[T3],
                                                                           private val ord4: Ordering[T4],
                                                                           private val ord5: Ordering[T5],
                                                                           private val ord6: Ordering[T6],
                                                                           private val ord7: Ordering[T7],
                                                                           private val ord8: Ordering[T8])
    extends Ordering[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    def compare(x: (T1, T2, T3, T4, T5, T6, T7, T8), y: (T1, T2, T3, T4, T5, T6, T7, T8)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      val compare6 = ord6.compare(x._6, y._6)
      if (compare6 != 0) return compare6
      val compare7 = ord7.compare(x._7, y._7)
      if (compare7 != 0) return compare7
      ord8.compare(x._8, y._8)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple8Ordering[_, _, _, _, _, _, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6 &&
        this.ord7 == that.ord7 &&
        this.ord8 == that.ord8
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8).hashCode()
  }

  @SerialVersionUID(8185342054829975001L)
  implicit def Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit ord1: Ordering[T1], ord2: Ordering[T2], ord3: Ordering[T3], ord4: Ordering[T4], ord5: Ordering[T5], ord6: Ordering[T6], ord7: Ordering[T7], ord8 : Ordering[T8], ord9: Ordering[T9]): Ordering[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    new Tuple9Ordering(ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8, ord9)

  private[this] final class Tuple9Ordering[T1, T2, T3, T4, T5, T6, T7, T8, T9](private val ord1: Ordering[T1],
                                                                               private val ord2: Ordering[T2],
                                                                               private val ord3: Ordering[T3],
                                                                               private val ord4: Ordering[T4],
                                                                               private val ord5: Ordering[T5],
                                                                               private val ord6: Ordering[T6],
                                                                               private val ord7: Ordering[T7],
                                                                               private val ord8: Ordering[T8],
                                                                               private val ord9: Ordering[T9])
    extends Ordering[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    def compare(x: (T1, T2, T3, T4, T5, T6, T7, T8, T9), y: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Int = {
      val compare1 = ord1.compare(x._1, y._1)
      if (compare1 != 0) return compare1
      val compare2 = ord2.compare(x._2, y._2)
      if (compare2 != 0) return compare2
      val compare3 = ord3.compare(x._3, y._3)
      if (compare3 != 0) return compare3
      val compare4 = ord4.compare(x._4, y._4)
      if (compare4 != 0) return compare4
      val compare5 = ord5.compare(x._5, y._5)
      if (compare5 != 0) return compare5
      val compare6 = ord6.compare(x._6, y._6)
      if (compare6 != 0) return compare6
      val compare7 = ord7.compare(x._7, y._7)
      if (compare7 != 0) return compare7
      val compare8 = ord8.compare(x._8, y._8)
      if (compare8 != 0) return compare8
      ord9.compare(x._9, y._9)
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple9Ordering[_, _, _, _, _, _, _, _, _] =>
        this.ord1 == that.ord1 &&
        this.ord2 == that.ord2 &&
        this.ord3 == that.ord3 &&
        this.ord4 == that.ord4 &&
        this.ord5 == that.ord5 &&
        this.ord6 == that.ord6 &&
        this.ord7 == that.ord7 &&
        this.ord8 == that.ord8 &&
        this.ord9 == that.ord9
      case _ => false
    }
    override def hashCode(): Int = (ord1, ord2, ord3, ord4, ord5, ord6, ord7, ord8, ord9).hashCode()
  }
}
