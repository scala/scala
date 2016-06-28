/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import util.{FreqMap, Buildable, Buildable2}


sealed abstract class Arbitrary[T] {
  val arbitrary: Gen[T]
}

/** Defines implicit [[org.scalacheck.Arbitrary]] instances for common types.
 *  <p>
 *  ScalaCheck
 *  uses implicit [[org.scalacheck.Arbitrary]] instances when creating properties
 *  out of functions with the `Prop.property` method, and when
 *  the `Arbitrary.arbitrary` method is used. For example, the
 *  following code requires that there exists an implicit
 *  `Arbitrary[MyClass]` instance:
 *  </p>
 *
 *  {{{
 *    val myProp = Prop.forAll { myClass: MyClass =>
 *      ...
 *    }
 *
 *    val myGen = Arbitrary.arbitrary[MyClass]
 *  }}}
 *
 *  <p>
 *  The required implicit definition could look like this:
 *  </p>
 *
 *  {{{
 *    implicit val arbMyClass: Arbitrary[MyClass] = Arbitrary(...)
 *  }}}
 *
 *  <p>
 *  The factory method `Arbitrary(...)` takes a generator of type
 *  `Gen[T]` and returns an instance of `Arbitrary[T]`.
 *  </p>
 *
 *  <p>
 *  The `Arbitrary` module defines implicit [[org.scalacheck.Arbitrary]]
 *  instances for common types, for convenient use in your properties and
 *  generators.
 *  </p>
 */
object Arbitrary {

  import Gen.{const, choose, sized, frequency, oneOf, containerOf, resize}
  import collection.{immutable, mutable}
  import java.util.Date

  /** Creates an Arbitrary instance */
  def apply[T](g: => Gen[T]): Arbitrary[T] = new Arbitrary[T] {
    lazy val arbitrary = g
  }

  /** Returns an arbitrary generator for the type T. */
  def arbitrary[T](implicit a: Arbitrary[T]): Gen[T] = a.arbitrary

  /**** Arbitrary instances for each AnyVal ****/

  /** Arbitrary AnyVal */
  implicit lazy val arbAnyVal: Arbitrary[AnyVal] = Arbitrary(oneOf(
    arbitrary[Unit], arbitrary[Boolean], arbitrary[Char], arbitrary[Byte],
    arbitrary[Short], arbitrary[Int], arbitrary[Long], arbitrary[Float],
    arbitrary[Double]
  ))

  /** Arbitrary instance of Boolean */
  implicit lazy val arbBool: Arbitrary[Boolean] =
    Arbitrary(oneOf(true, false))

  /** Arbitrary instance of Int */
  implicit lazy val arbInt: Arbitrary[Int] = Arbitrary(
    Gen.chooseNum(Int.MinValue, Int.MaxValue)
  )

  /** Arbitrary instance of Long */
  implicit lazy val arbLong: Arbitrary[Long] = Arbitrary(
    Gen.chooseNum(Long.MinValue, Long.MaxValue)
  )

  /** Arbitrary instance of Float */
  implicit lazy val arbFloat: Arbitrary[Float] = Arbitrary(
    Gen.chooseNum(
      Float.MinValue, Float.MaxValue
      // I find that including these by default is a little TOO testy.
      // Float.Epsilon, Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity
    )
  )

  /** Arbitrary instance of Double */
  implicit lazy val arbDouble: Arbitrary[Double] = Arbitrary(
    Gen.chooseNum(
      Double.MinValue / 2, Double.MaxValue / 2
      // As above.  Perhaps behind some option?
      // Double.Epsilon, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity
    )
  )

  /** Arbitrary instance of Char */
  implicit lazy val arbChar: Arbitrary[Char] = Arbitrary(
    Gen.frequency(
      (0xD800-Char.MinValue, Gen.choose[Char](Char.MinValue,0xD800-1)),
      (Char.MaxValue-0xDFFF, Gen.choose[Char](0xDFFF+1,Char.MaxValue))
    )
  )

  /** Arbitrary instance of Byte */
  implicit lazy val arbByte: Arbitrary[Byte] = Arbitrary(
    Gen.chooseNum(Byte.MinValue, Byte.MaxValue)
  )

  /** Arbitrary instance of Short */
  implicit lazy val arbShort: Arbitrary[Short] = Arbitrary(
    Gen.chooseNum(Short.MinValue, Short.MaxValue)
  )

  /** Absolutely, totally, 100% arbitrarily chosen Unit. */
  implicit lazy val arbUnit: Arbitrary[Unit] = Arbitrary(const(()))

  /**** Arbitrary instances of other common types ****/

  /** Arbitrary instance of String */
  implicit lazy val arbString: Arbitrary[String] =
    Arbitrary(arbitrary[List[Char]] map (_.mkString))

  /** Arbitrary instance of Date */
  implicit lazy val arbDate: Arbitrary[Date] = Arbitrary(for {
    l <- arbitrary[Long]
    d = new Date
  } yield new Date(d.getTime + l))

  /** Arbitrary instance of Throwable */
  implicit lazy val arbThrowable: Arbitrary[Throwable] =
    Arbitrary(oneOf(const(new Exception), const(new Error)))

  /** Arbitrary instance of Exception */
  implicit lazy val arbException: Arbitrary[Exception] =
    Arbitrary(const(new Exception))

  /** Arbitrary instance of Error */
  implicit lazy val arbError: Arbitrary[Error] =
    Arbitrary(const(new Error))

  /** Arbitrary BigInt */
  implicit lazy val arbBigInt: Arbitrary[BigInt] = {
    def chooseBigInt: Gen[BigInt] =
      sized((s: Int) => choose(-s, s)) map (x => BigInt(x))

    def chooseReallyBigInt: Gen[BigInt] = for {
      bi <- chooseBigInt
      n <- choose(32,128)
    } yield bi << n

    Arbitrary(
      frequency(
        (5, chooseBigInt),
        (10, chooseReallyBigInt),
        (1, BigInt(0)),
        (1, BigInt(1)),
        (1, BigInt(-1)),
        (1, BigInt(Int.MaxValue) + 1),
        (1, BigInt(Int.MinValue) - 1),
        (1, BigInt(Long.MaxValue)),
        (1, BigInt(Long.MinValue)),
        (1, BigInt(Long.MaxValue) + 1),
        (1, BigInt(Long.MinValue) - 1)
      )
    )
  }

  /** Arbitrary BigDecimal */
  implicit lazy val arbBigDecimal: Arbitrary[BigDecimal] = {
    import java.math.MathContext._
    val mcGen = oneOf(UNLIMITED, DECIMAL32, DECIMAL64, DECIMAL128)
    val bdGen = for {
      x <- arbBigInt.arbitrary
      mc <- mcGen
      limit <- const(if(mc == UNLIMITED) 0 else math.max(x.abs.toString.length - mc.getPrecision, 0))
      scale <- Gen.chooseNum(Int.MinValue + limit , Int.MaxValue)
    } yield {
      try {
        BigDecimal(x, scale, mc)
      } catch {
        case ae: java.lang.ArithmeticException => BigDecimal(x, scale, UNLIMITED) // Handle the case where scale/precision conflict
      }
    }
    Arbitrary(bdGen)
  }

  /** Arbitrary java.lang.Number */
  implicit lazy val arbNumber: Arbitrary[Number] = {
    val gen = Gen.oneOf(
      arbitrary[Byte], arbitrary[Short], arbitrary[Int], arbitrary[Long],
      arbitrary[Float], arbitrary[Double]
    )
    Arbitrary(gen map (_.asInstanceOf[Number]))
    // XXX TODO - restore BigInt and BigDecimal
    // Arbitrary(oneOf(arbBigInt.arbitrary :: (arbs map (_.arbitrary) map toNumber) : _*))
  }

  /** Generates an arbitrary property */
  implicit lazy val arbProp: Arbitrary[Prop] = {
    import Prop._
    val undecidedOrPassed = forAll { b: Boolean =>
      b ==> true
    }
    Arbitrary(frequency(
      (4, falsified),
      (4, passed),
      (3, proved),
      (3, undecidedOrPassed),
      (2, undecided),
      (1, exception(null))
    ))
  }

  /** Arbitrary instance of test parameters */
  implicit lazy val arbTestParameters: Arbitrary[Test.Parameters] =
    Arbitrary(for {
      _minSuccTests <- choose(10,200)
      _maxDiscardRatio <- choose(0.2f,10f)
      _minSize <- choose(0,500)
      sizeDiff <- choose(0,500)
      _maxSize <- choose(_minSize, _minSize + sizeDiff)
      _workers <- choose(1,4)
    } yield new Test.Parameters.Default {
      override val minSuccessfulTests = _minSuccTests
      override val maxDiscardRatio = _maxDiscardRatio
      override val minSize = _minSize
      override val maxSize = _maxSize
      override val workers = _workers
    })

  /** Arbitrary instance of gen params */
  implicit lazy val arbGenParams: Arbitrary[Gen.Parameters] =
    Arbitrary(for {
      sz <- arbitrary[Int] suchThat (_ >= 0)
    } yield (new Gen.Parameters.Default {
      override val size = sz
    }))


  // Higher-order types //

  /** Arbitrary instance of [[org.scalacheck.Gen]] */
  implicit def arbGen[T](implicit a: Arbitrary[T]): Arbitrary[Gen[T]] =
    Arbitrary(frequency(
      (5, arbitrary[T] map (const(_))),
      (1, Gen.fail)
    ))

  /** Arbitrary instance of the Option type */
  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary(sized(n =>
      // When n is larger, make it less likely that we generate None,
      // but still do it some of the time. When n is zero, we always
      // generate None, since it's the smallest value.
      frequency(
        (n, resize(n / 2, arbitrary[T]).map(Some(_))),
        (1, const(None)))))

  /** Arbitrary instance of the Either type */
  implicit def arbEither[T, U](implicit at: Arbitrary[T], au: Arbitrary[U]): Arbitrary[Either[T, U]] =
    Arbitrary(oneOf(arbitrary[T].map(Left(_)), arbitrary[U].map(Right(_))))

  /** Arbitrary instance of any [[org.scalacheck.util.Buildable]] container
   *  (such as lists, arrays, streams, etc). The maximum size of the container
   *  depends on the size generation parameter. */
  implicit def arbContainer[C[_],T](implicit
    a: Arbitrary[T], b: Buildable[T,C], t: C[T] => Traversable[T]
  ): Arbitrary[C[T]] = Arbitrary(containerOf[C,T](arbitrary[T]))

  /** Arbitrary instance of any [[org.scalacheck.util.Buildable2]] container
   *  (such as maps, etc). The maximum size of the container depends on the size
   *  generation parameter. */
  implicit def arbContainer2[C[_,_],T,U](implicit
    a: Arbitrary[(T,U)], b: Buildable2[T,U,C], t: C[T,U] => Traversable[(T,U)]
  ): Arbitrary[C[T,U]] = Arbitrary(containerOf[C,T,U](arbitrary[(T,U)]))

  // Functions //

  /** Arbitrary instance of Function1 */
  implicit def arbFunction1[T1,R](implicit a: Arbitrary[R]
  ): Arbitrary[T1 => R] = Arbitrary(
    for(r <- arbitrary[R]) yield (t1: T1) => r
  )

  /** Arbitrary instance of Function2 */
  implicit def arbFunction2[T1,T2,R](implicit a: Arbitrary[R]
  ): Arbitrary[(T1,T2) => R] = Arbitrary(
    for(r <- arbitrary[R]) yield (t1: T1, t2: T2) => r
  )

  /** Arbitrary instance of Function3 */
  implicit def arbFunction3[T1,T2,T3,R](implicit a: Arbitrary[R]
  ): Arbitrary[(T1,T2,T3) => R] = Arbitrary(
    for(r <- arbitrary[R]) yield (t1: T1, t2: T2, t3: T3) => r
  )

  /** Arbitrary instance of Function4 */
  implicit def arbFunction4[T1,T2,T3,T4,R](implicit a: Arbitrary[R]
  ): Arbitrary[(T1,T2,T3,T4) => R] = Arbitrary(
    for(r <- arbitrary[R]) yield (t1: T1, t2: T2, t3: T3, t4: T4) => r
  )

  /** Arbitrary instance of Function5 */
  implicit def arbFunction5[T1,T2,T3,T4,T5,R](implicit a: Arbitrary[R]
  ): Arbitrary[(T1,T2,T3,T4,T5) => R] = Arbitrary(
    for(r <- arbitrary[R]) yield (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => r
  )


  // Tuples //

  /** Arbitrary instance of 2-tuple */
  implicit def arbTuple2[T1,T2](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2]
  ): Arbitrary[(T1,T2)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
    } yield (t1,t2))

  /** Arbitrary instance of 3-tuple */
  implicit def arbTuple3[T1,T2,T3](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]
  ): Arbitrary[(T1,T2,T3)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
    } yield (t1,t2,t3))

  /** Arbitrary instance of 4-tuple */
  implicit def arbTuple4[T1,T2,T3,T4](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]
  ): Arbitrary[(T1,T2,T3,T4)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
      t4 <- arbitrary[T4]
    } yield (t1,t2,t3,t4))

  /** Arbitrary instance of 5-tuple */
  implicit def arbTuple5[T1,T2,T3,T4,T5](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5]
  ): Arbitrary[(T1,T2,T3,T4,T5)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
      t4 <- arbitrary[T4]
      t5 <- arbitrary[T5]
    } yield (t1,t2,t3,t4,t5))

  /** Arbitrary instance of 6-tuple */
  implicit def arbTuple6[T1,T2,T3,T4,T5,T6](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6]
  ): Arbitrary[(T1,T2,T3,T4,T5,T6)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
      t4 <- arbitrary[T4]
      t5 <- arbitrary[T5]
      t6 <- arbitrary[T6]
    } yield (t1,t2,t3,t4,t5,t6))

  /** Arbitrary instance of 7-tuple */
  implicit def arbTuple7[T1,T2,T3,T4,T5,T6,T7](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]
  ): Arbitrary[(T1,T2,T3,T4,T5,T6,T7)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
      t4 <- arbitrary[T4]
      t5 <- arbitrary[T5]
      t6 <- arbitrary[T6]
      t7 <- arbitrary[T7]
    } yield (t1,t2,t3,t4,t5,t6,t7))

  /** Arbitrary instance of 8-tuple */
  implicit def arbTuple8[T1,T2,T3,T4,T5,T6,T7,T8](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]
  ): Arbitrary[(T1,T2,T3,T4,T5,T6,T7,T8)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
      t4 <- arbitrary[T4]
      t5 <- arbitrary[T5]
      t6 <- arbitrary[T6]
      t7 <- arbitrary[T7]
      t8 <- arbitrary[T8]
    } yield (t1,t2,t3,t4,t5,t6,t7,t8))

  /** Arbitrary instance of 9-tuple */
  implicit def arbTuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9](implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8],
    a9: Arbitrary[T9]
  ): Arbitrary[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] =
    Arbitrary(for {
      t1 <- arbitrary[T1]
      t2 <- arbitrary[T2]
      t3 <- arbitrary[T3]
      t4 <- arbitrary[T4]
      t5 <- arbitrary[T5]
      t6 <- arbitrary[T6]
      t7 <- arbitrary[T7]
      t8 <- arbitrary[T8]
      t9 <- arbitrary[T9]
    } yield (t1,t2,t3,t4,t5,t6,t7,t8,t9))

}
