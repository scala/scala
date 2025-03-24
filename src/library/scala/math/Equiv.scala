/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
import scala.annotation.migration

/** A trait for representing equivalence relations.  It is important to
 *  distinguish between a type that can be compared for equality or
 *  equivalence and a representation of equivalence on some type. This
 *  trait is for representing the latter.
 *
 *  An [[https://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]
 *  is a binary relation on a type. This relation is exposed as
 *  the `equiv` method of the `Equiv` trait.  The relation must be:
 *
 *    1. reflexive: `equiv(x, x) == true` for any x of type `T`.
 *    1. symmetric: `equiv(x, y) == equiv(y, x)` for any `x` and `y` of type `T`.
 *    1. transitive: if `equiv(x, y) == true` and `equiv(y, z) == true`, then
 *       `equiv(x, z) == true` for any `x`, `y`, and `z` of type `T`.
 */

trait Equiv[T] extends Any with Serializable {
  /** Returns `true` iff `x` is equivalent to `y`.
   */
  def equiv(x: T, y: T): Boolean
}

trait LowPriorityEquiv {
  self: Equiv.type =>

  /**
   * @deprecated This implicit universal `Equiv` instance allows accidentally
   * comparing instances of types for which equality isn't well-defined or implemented.
   * (For example, it does not make sense to compare two `Function1` instances.)
   *
   * Use `Equiv.universal` explicitly instead. If you really want an implicit universal `Equiv` instance
   * despite the potential problems, consider `implicit def universalEquiv[T]: Equiv[T] = universal[T]`.
   */
  @deprecated("Use explicit Equiv.universal instead. See Scaladoc entry for more information: " +
    "https://www.scala-lang.org/api/current/scala/math/Equiv$.html#universalEquiv[T]:scala.math.Equiv[T]",
    since = "2.13.0")
  implicit def universalEquiv[T]: Equiv[T] = universal[T]
}

object Equiv extends LowPriorityEquiv {
  def reference[T <: AnyRef]: Equiv[T] = { _ eq _ }
  def universal[T]: Equiv[T] = { _ == _ }
  def fromComparator[T](cmp: Comparator[T]): Equiv[T] = {
    (x, y) => cmp.compare(x, y) == 0
  }
  def fromFunction[T](cmp: (T, T) => Boolean): Equiv[T] = {
    (x, y) => cmp(x, y)
  }
  def by[T, S: Equiv](f: T => S): Equiv[T] =
    ((x, y) => implicitly[Equiv[S]].equiv(f(x), f(y)))

  @inline def apply[T: Equiv]: Equiv[T] = implicitly[Equiv[T]]

  /* copied from Ordering */

  private final val optionSeed   = 43
  private final val iterableSeed = 47

  private final class IterableEquiv[CC[X] <: Iterable[X], T](private val eqv: Equiv[T]) extends Equiv[CC[T]] {
    def equiv(x: CC[T], y: CC[T]): Boolean = {
      val xe = x.iterator
      val ye = y.iterator

      while (xe.hasNext && ye.hasNext) {
        if (!eqv.equiv(xe.next(), ye.next())) return false
      }

      xe.hasNext == ye.hasNext
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that  => true
      case that: IterableEquiv[_, _]     => this.eqv == that.eqv
      case _                             => false
    }
    override def hashCode(): Int = eqv.hashCode() * iterableSeed
  }

  trait ExtraImplicits {
    /** Not in the standard scope due to the potential for divergence:
      * For instance `implicitly[Equiv[Any]]` diverges in its presence.
      */
    implicit def seqEquiv[CC[X] <: scala.collection.Seq[X], T](implicit eqv: Equiv[T]): Equiv[CC[T]] =
      new IterableEquiv[CC, T](eqv)

    implicit def sortedSetEquiv[CC[X] <: scala.collection.SortedSet[X], T](implicit eqv: Equiv[T]): Equiv[CC[T]] =
      new IterableEquiv[CC, T](eqv)
  }

  /** An object containing implicits which are not in the default scope. */
  object Implicits extends ExtraImplicits { }

  implicit object Unit extends Equiv[Unit] {
    def equiv(x: Unit, y: Unit): Boolean = true
  }

  implicit object Boolean extends Equiv[Boolean] {
    def equiv(x: Boolean, y: Boolean): Boolean = x == y
  }

  implicit object Byte extends Equiv[Byte] {
    def equiv(x: Byte, y: Byte): Boolean = x == y
  }

  implicit object Char extends Equiv[Char] {
    def equiv(x: Char, y: Char): Boolean = x == y
  }

  implicit object Short extends Equiv[Short] {
    def equiv(x: Short, y: Short): Boolean = x == y
  }

  implicit object Int extends Equiv[Int] {
    def equiv(x: Int, y: Int): Boolean = x == y
  }

  implicit object Long extends Equiv[Long] {
    def equiv(x: Long, y: Long): Boolean = x == y
  }

  /** `Equiv`s for `Float`s.
    *
    * @define floatEquiv Because the behaviour of `Float`s specified by IEEE is
    *                    not consistent with behaviors required of an equivalence
    *                    relation for `NaN` (it is not reflexive), there are two
    *                    equivalences defined for `Float`: `StrictEquiv`, which
    *                    is reflexive, and `IeeeEquiv`, which is consistent
    *                    with IEEE spec and floating point operations defined in
    *                    [[scala.math]].
    */
  object Float {
    /** An equivalence for `Float`s which is reflexive (treats all `NaN`s
      * as equivalent), and treats `-0.0` and `0.0` as not equivalent; it
      * behaves the same as [[java.lang.Float.compare]].
      *
      * $floatEquiv
      *
      * This equivalence may be preferable for collections.
      *
      * @see [[IeeeEquiv]]
      */
    trait StrictEquiv extends Equiv[Float] {
      def equiv(x: Float, y: Float): Boolean = java.lang.Float.compare(x, y) == 0
    }
    implicit object StrictEquiv extends StrictEquiv

    /** An equivalence for `Float`s which is consistent with IEEE specifications.
      *
      * $floatEquiv
      *
      * This equivalence may be preferable for numeric contexts.
      *
      * @see [[StrictEquiv]]
      */
    trait IeeeEquiv extends Equiv[Float] {
      override def equiv(x: Float, y: Float): Boolean = x == y
    }
    implicit object IeeeEquiv extends IeeeEquiv
  }

  @migration(
    "  The default implicit equivalence for floats no longer conforms to\n" +
    "  to IEEE 754's behavior for -0.0F and NaN.\n" +
    "  Import `Equiv.Float.IeeeEquiv` to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Equiv$$Float$.html.", "2.13.2")
  implicit object DeprecatedFloatEquiv extends Float.StrictEquiv

  /** `Equiv`s for `Double`s.
    *
    * @define doubleEquiv Because the behaviour of `Double`s specified by IEEE is
    *                     not consistent with behaviors required of an equivalence
    *                     relation for `NaN` (it is not reflexive), there are two
    *                     equivalences defined for `Double`: `StrictEquiv`, which
    *                     is reflexive, and `IeeeEquiv`, which is consistent
    *                     with IEEE spec and floating point operations defined in
    *                     [[scala.math]].
    */
  object Double {
    /** An equivalence for `Double`s which is reflexive (treats all `NaN`s
      * as equivalent), and treats `-0.0` and `0.0` as not equivalent; it
      * behaves the same as [[java.lang.Double.compare]].
      *
      * $doubleEquiv
      *
      * This equivalence may be preferable for collections.
      *
      * @see [[IeeeEquiv]]
      */
    trait StrictEquiv extends Equiv[Double] {
      def equiv(x: Double, y: Double): Boolean = java.lang.Double.compare(x, y) == 0
    }
    implicit object StrictEquiv extends StrictEquiv

    /** An equivalence for `Double`s which is consistent with IEEE specifications.
      *
      * $doubleEquiv
      *
      * This equivalence may be preferable for numeric contexts.
      *
      * @see [[StrictEquiv]]
      */
    trait IeeeEquiv extends Equiv[Double] {
      def equiv(x: Double, y: Double): Boolean = x == y
    }
    implicit object IeeeEquiv extends IeeeEquiv
  }
  @migration(
    "  The default implicit equivalence for doubles no longer conforms to\n" +
    "  to IEEE 754's behavior for -0.0D and NaN.\n" +
    "  Import `Equiv.Double.IeeeEquiv` to recover the previous behavior.\n" +
    "  See also https://www.scala-lang.org/api/current/scala/math/Equiv$$Double$.html.", "2.13.2")
  implicit object DeprecatedDoubleEquiv extends Double.StrictEquiv

  implicit object BigInt extends Equiv[BigInt] {
    def equiv(x: BigInt, y: BigInt): Boolean = x == y
  }

  implicit object BigDecimal extends Equiv[BigDecimal] {
    def equiv(x: BigDecimal, y: BigDecimal): Boolean = x == y
  }

  implicit object String extends Equiv[String] {
    def equiv(x: String, y: String): Boolean = x == y
  }

  implicit object Symbol extends Equiv[Symbol] {
    def equiv(x: Symbol, y: Symbol): Boolean = x == y
  }

  implicit def Option[T](implicit eqv: Equiv[T]): Equiv[Option[T]] =
    new OptionEquiv[T](eqv)

  private[this] final class OptionEquiv[T](private val eqv: Equiv[T]) extends Equiv[Option[T]] {
    def equiv(x: Option[T], y: Option[T]): Boolean = (x, y) match {
      case (None, None)       => true
      case (Some(x), Some(y)) => eqv.equiv(x, y)
      case _                  => false
    }

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: OptionEquiv[_]         => this.eqv == that.eqv
      case _                            => false
    }
    override def hashCode(): Int = eqv.hashCode() * optionSeed
  }

  implicit def Tuple2[T1, T2](implicit eqv1: Equiv[T1], eqv2: Equiv[T2]): Equiv[(T1, T2)] =
    new Tuple2Equiv(eqv1, eqv2)

  private[this] final class Tuple2Equiv[T1, T2](private val eqv1: Equiv[T1],
                                                private val eqv2: Equiv[T2]) extends Equiv[(T1, T2)] {
    def equiv(x: (T1, T2), y: (T1, T2)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple2Equiv[_, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2).hashCode()
  }

  implicit def Tuple3[T1, T2, T3](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3]) : Equiv[(T1, T2, T3)] =
    new Tuple3Equiv(eqv1, eqv2, eqv3)

  private[this] final class Tuple3Equiv[T1, T2, T3](private val eqv1: Equiv[T1],
                                                    private val eqv2: Equiv[T2],
                                                    private val eqv3: Equiv[T3]) extends Equiv[(T1, T2, T3)] {
    def equiv(x: (T1, T2, T3), y: (T1, T2, T3)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple3Equiv[_, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3).hashCode()
  }

  implicit def Tuple4[T1, T2, T3, T4](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4]) : Equiv[(T1, T2, T3, T4)] =
    new Tuple4Equiv(eqv1, eqv2, eqv3, eqv4)

  private[this] final class Tuple4Equiv[T1, T2, T3, T4](private val eqv1: Equiv[T1],
                                                        private val eqv2: Equiv[T2],
                                                        private val eqv3: Equiv[T3],
                                                        private val eqv4: Equiv[T4])
    extends Equiv[(T1, T2, T3, T4)] {
    def equiv(x: (T1, T2, T3, T4), y: (T1, T2, T3, T4)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple4Equiv[_, _, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4).hashCode()
  }

  implicit def Tuple5[T1, T2, T3, T4, T5](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5]): Equiv[(T1, T2, T3, T4, T5)] =
    new Tuple5Equiv(eqv1, eqv2, eqv3, eqv4, eqv5)

  private[this] final class Tuple5Equiv[T1, T2, T3, T4, T5](private val eqv1: Equiv[T1],
                                                            private val eqv2: Equiv[T2],
                                                            private val eqv3: Equiv[T3],
                                                            private val eqv4: Equiv[T4],
                                                            private val eqv5: Equiv[T5])
    extends Equiv[(T1, T2, T3, T4, T5)] {
    def equiv(x: (T1, T2, T3, T4, T5), y: (T1, T2, T3, T4, T5)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple5Equiv[_, _, _, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5).hashCode()
  }

  implicit def Tuple6[T1, T2, T3, T4, T5, T6](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6]): Equiv[(T1, T2, T3, T4, T5, T6)] =
    new Tuple6Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6)

  private[this] final class Tuple6Equiv[T1, T2, T3, T4, T5, T6](private val eqv1: Equiv[T1],
                                                                private val eqv2: Equiv[T2],
                                                                private val eqv3: Equiv[T3],
                                                                private val eqv4: Equiv[T4],
                                                                private val eqv5: Equiv[T5],
                                                                private val eqv6: Equiv[T6])
    extends Equiv[(T1, T2, T3, T4, T5, T6)] {
    def equiv(x: (T1, T2, T3, T4, T5, T6), y: (T1, T2, T3, T4, T5, T6)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple6Equiv[_, _, _, _, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6).hashCode()
  }

  implicit def Tuple7[T1, T2, T3, T4, T5, T6, T7](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6], eqv7: Equiv[T7]): Equiv[(T1, T2, T3, T4, T5, T6, T7)] =
    new Tuple7Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7)

  private[this] final class Tuple7Equiv[T1, T2, T3, T4, T5, T6, T7](private val eqv1: Equiv[T1],
                                                                    private val eqv2: Equiv[T2],
                                                                    private val eqv3: Equiv[T3],
                                                                    private val eqv4: Equiv[T4],
                                                                    private val eqv5: Equiv[T5],
                                                                    private val eqv6: Equiv[T6],
                                                                    private val eqv7: Equiv[T7])
    extends Equiv[(T1, T2, T3, T4, T5, T6, T7)] {
    def equiv(x: (T1, T2, T3, T4, T5, T6, T7), y: (T1, T2, T3, T4, T5, T6, T7)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6) &&
      eqv7.equiv(x._7, y._7)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple7Equiv[_, _, _, _, _, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6 &&
        this.eqv7 == that.eqv7
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7).hashCode()
  }

  implicit def Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6], eqv7: Equiv[T7], eqv8: Equiv[T8]): Equiv[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    new Tuple8Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8)

  private[this] final class Tuple8Equiv[T1, T2, T3, T4, T5, T6, T7, T8](private val eqv1: Equiv[T1],
                                                                        private val eqv2: Equiv[T2],
                                                                        private val eqv3: Equiv[T3],
                                                                        private val eqv4: Equiv[T4],
                                                                        private val eqv5: Equiv[T5],
                                                                        private val eqv6: Equiv[T6],
                                                                        private val eqv7: Equiv[T7],
                                                                        private val eqv8: Equiv[T8])
    extends Equiv[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    def equiv(x: (T1, T2, T3, T4, T5, T6, T7, T8), y: (T1, T2, T3, T4, T5, T6, T7, T8)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6) &&
      eqv7.equiv(x._7, y._7) &&
      eqv8.equiv(x._8, y._8)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple8Equiv[_, _, _, _, _, _, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6 &&
        this.eqv7 == that.eqv7 &&
        this.eqv8 == that.eqv8
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8).hashCode()
  }

  implicit def Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit eqv1: Equiv[T1], eqv2: Equiv[T2], eqv3: Equiv[T3], eqv4: Equiv[T4], eqv5: Equiv[T5], eqv6: Equiv[T6], eqv7: Equiv[T7], eqv8 : Equiv[T8], eqv9: Equiv[T9]): Equiv[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    new Tuple9Equiv(eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8, eqv9)

  private[this] final class Tuple9Equiv[T1, T2, T3, T4, T5, T6, T7, T8, T9](private val eqv1: Equiv[T1],
                                                                            private val eqv2: Equiv[T2],
                                                                            private val eqv3: Equiv[T3],
                                                                            private val eqv4: Equiv[T4],
                                                                            private val eqv5: Equiv[T5],
                                                                            private val eqv6: Equiv[T6],
                                                                            private val eqv7: Equiv[T7],
                                                                            private val eqv8: Equiv[T8],
                                                                            private val eqv9: Equiv[T9])
    extends Equiv[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    def equiv(x: (T1, T2, T3, T4, T5, T6, T7, T8, T9), y: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Boolean =
      eqv1.equiv(x._1, y._1) &&
      eqv2.equiv(x._2, y._2) &&
      eqv3.equiv(x._3, y._3) &&
      eqv4.equiv(x._4, y._4) &&
      eqv5.equiv(x._5, y._5) &&
      eqv6.equiv(x._6, y._6) &&
      eqv7.equiv(x._7, y._7) &&
      eqv8.equiv(x._8, y._8) &&
      eqv9.equiv(x._9, y._9)

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: Tuple9Equiv[_, _, _, _, _, _, _, _, _] =>
        this.eqv1 == that.eqv1 &&
        this.eqv2 == that.eqv2 &&
        this.eqv3 == that.eqv3 &&
        this.eqv4 == that.eqv4 &&
        this.eqv5 == that.eqv5 &&
        this.eqv6 == that.eqv6 &&
        this.eqv7 == that.eqv7 &&
        this.eqv8 == that.eqv8 &&
        this.eqv9 == that.eqv9
      case _ => false
    }
    override def hashCode(): Int = (eqv1, eqv2, eqv3, eqv4, eqv5, eqv6, eqv7, eqv8, eqv9).hashCode()
  }

}
