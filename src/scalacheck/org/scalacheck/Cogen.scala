/*-------------------------------------------------------------------------*\
 **  ScalaCheck                                                             **
 **  Copyright (c) 2007-2017 Rickard Nilsson. All rights reserved.          **
 **  http://www.scalacheck.org                                              **
 **                                                                         **
 **  This software is released under the terms of the Revised BSD License.  **
 **  There is NO WARRANTY. See the file LICENSE for the full text.          **
 \*------------------------------------------------------------------------ */

package org.scalacheck

import language.higherKinds
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.Try.{Success, Failure}
import scala.concurrent.duration.{Duration, FiniteDuration}
import java.math.BigInteger
import rng.Seed

sealed trait Cogen[T] extends Serializable {

  def perturb(seed: Seed, t: T): Seed

  def cogen[A](t: T, g: Gen[A]): Gen[A] =
    Gen.gen((p, seed) => g.doApply(p, perturb(seed, t)))

  def contramap[S](f: S => T): Cogen[S] =
    Cogen((seed: Seed, s: S) => perturb(seed, f(s)))
}

object Cogen extends CogenArities with CogenLowPriority {

  // See https://github.com/rickynils/scalacheck/issues/230 for dummy expl.
  def apply[T](implicit ev: Cogen[T], dummy: Cogen[T]): Cogen[T] = ev

  def apply[T](f: T => Long): Cogen[T] = new Cogen[T] {
    def perturb(seed: Seed, t: T): Seed = seed.reseed(f(t))
  }

  def apply[T](f: (Seed, T) => Seed): Cogen[T] =
    new Cogen[T] {
      def perturb(seed: Seed, t: T): Seed = f(seed, t)
    }

  def it[T, U](f: T => Iterator[U])(implicit U: Cogen[U]): Cogen[T] =
    new Cogen[T] {
      def perturb(seed: Seed, t: T): Seed =
        f(t).foldLeft(seed)(U.perturb).next
    }

  def perturb[T](seed: Seed, t: T)(implicit cg: Cogen[T]): Seed =
    cg.perturb(seed, t)

  implicit lazy val cogenUnit: Cogen[Unit] = Cogen(_ => 0L)

  implicit lazy val cogenBoolean: Cogen[Boolean] =
    Cogen(b => if (b) 1L else 0L)

  implicit lazy val cogenByte: Cogen[Byte] = Cogen(_.toLong)
  implicit lazy val cogenShort: Cogen[Short] = Cogen(_.toLong)
  implicit lazy val cogenChar: Cogen[Char] = Cogen(_.toLong)
  implicit lazy val cogenInt: Cogen[Int] = Cogen(_.toLong)
  implicit lazy val cogenLong: Cogen[Long] = Cogen(n => n)

  implicit lazy val cogenFloat: Cogen[Float] =
    Cogen(n => java.lang.Float.floatToIntBits(n).toLong)

  implicit lazy val cogenDouble: Cogen[Double] =
    Cogen(n => java.lang.Double.doubleToLongBits(n))

  implicit lazy val bigInt: Cogen[BigInt] =
    Cogen[Array[Byte]].contramap(_.toByteArray)

  implicit lazy val bigDecimal: Cogen[BigDecimal] = {

    // Normalize unscaled values and scaling factors by moving powers of ten from value to scaling factor.
    @tailrec
    def normalize(unscaled: BigInteger, scale: Int): (BigInteger, Int) = {
      val divideAndRemainder = unscaled.divideAndRemainder(BigInteger.TEN)
      val quotient = divideAndRemainder(0)
      val remainder = divideAndRemainder(1)
      val canNormalize = (unscaled.abs.compareTo(BigInteger.TEN) >= 0) &&
        (remainder == BigInteger.ZERO) &&
        (scale != Int.MaxValue) &&
        (scale != Int.MinValue)
      if (canNormalize) normalize(quotient, scale - 1) else (unscaled, scale)
    }

    // If the unscaled value is zero then the scaling factor doesn't matter. Otherwise perturb based on both.
    Cogen((seed: Seed, n: BigDecimal) =>
      if (n.bigDecimal.unscaledValue == BigInteger.ZERO)
        Cogen[Int].perturb(seed, 0)
      else {
        val (unscaled, scale) = normalize(n.bigDecimal.unscaledValue, n.scale)
        Cogen[(Int, Array[Byte])].perturb(seed, (scale, unscaled.toByteArray))
    })
  }

  implicit lazy val bitSet: Cogen[BitSet] =
    Cogen.it(_.iterator)

  implicit def cogenOption[A](implicit A: Cogen[A]): Cogen[Option[A]] =
    Cogen((seed: Seed, o: Option[A]) => o.fold(seed)(a => A.perturb(seed.next, a)))

  implicit def cogenEither[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[Either[A, B]] =
    Cogen((seed: Seed, e: Either[A,B]) => e.fold(a => A.perturb(seed, a), b => B.perturb(seed.next, b)))

  implicit def cogenArray[A](implicit A: Cogen[A]): Cogen[Array[A]] =
    Cogen((seed: Seed, as: Array[A]) => perturbArray(seed, as))

  implicit def cogenString: Cogen[String] =
    Cogen.it(_.iterator)

  implicit def cogenSymbol: Cogen[Symbol] =
    Cogen[String].contramap(_.name)

  implicit def cogenList[A: Cogen]: Cogen[List[A]] =
    Cogen.it(_.iterator)

  implicit def cogenVector[A: Cogen]: Cogen[Vector[A]] =
    Cogen.it(_.iterator)

  implicit def cogenStream[A: Cogen]: Cogen[Stream[A]] =
    Cogen.it(_.iterator)

  implicit def cogenSet[A: Cogen: Ordering]: Cogen[Set[A]] =
    Cogen.it(_.toVector.sorted.iterator)

  implicit def cogenMap[K: Cogen: Ordering, V: Cogen]: Cogen[Map[K, V]] =
    Cogen.it(_.toVector.sortBy(_._1).iterator)

  implicit def cogenFunction0[Z: Cogen]: Cogen[() => Z] =
    Cogen[Z].contramap(f => f())

  implicit val cogenException: Cogen[Exception] =
    Cogen[String].contramap(_.toString)

  implicit val cogenThrowable: Cogen[Throwable] =
    Cogen[String].contramap(_.toString)

  implicit def cogenTry[A: Cogen]: Cogen[Try[A]] =
    Cogen((seed: Seed, x: Try[A]) => x match {
      case Success(a) => Cogen[A].perturb(seed.next, a)
      case Failure(e) => Cogen[Throwable].perturb(seed, e)
    })

  implicit val cogenFiniteDuration: Cogen[FiniteDuration] =
    Cogen[Long].contramap(_.toNanos)

  implicit val cogenDuration: Cogen[Duration] =
    Cogen((seed: Seed, x: Duration) => x match {
      case d: FiniteDuration => Cogen[FiniteDuration].perturb(seed, d)
      // Undefined -> NaN, Inf -> PositiveInfinity, MinusInf -> NegativeInf
      // We could just use `toUnit` for finite durations too, but the Long => Double
      // conversion is lossy, so this approach may be better.
      case d => Cogen[Double].perturb(seed, d.toUnit(java.util.concurrent.TimeUnit.NANOSECONDS))
    })

  implicit def cogenPartialFunction[A: Arbitrary, B: Cogen]: Cogen[PartialFunction[A, B]] =
    Cogen[A => Option[B]].contramap(_.lift)

  def perturbPair[A, B](seed: Seed, ab: (A, B))(implicit A: Cogen[A], B: Cogen[B]): Seed =
    B.perturb(A.perturb(seed, ab._1), ab._2)

  def perturbArray[A](seed: Seed, as: Array[A])(implicit A: Cogen[A]): Seed = {
    var s = seed
    var i = 0
    while (i < as.length) { s = A.perturb(s, as(i)); i += 1 }
    s.next
  }
}

trait CogenLowPriority {
  implicit def cogenSeq[CC[x] <: Seq[x], A: Cogen]: Cogen[CC[A]] =
    Cogen.it(_.iterator)
}
