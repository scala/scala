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

package scala.jdk

import org.scalacheck.{Arbitrary, Gen, Properties}
import Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import scala.util.Random

object AccumulatorTest extends Properties("Accumulators") {
  property("IntAccumulatorConvert") = forAll(
    arbitrary[Seq[Int]]
  ) { xs =>
    val acc: IntAccumulator = Accumulator(xs: _*)
    acc.toList == xs &&
      acc.stepper.iterator.toList == xs &&
      acc.toArray.toList == xs &&
      acc.to(List) == xs &&
      acc.iterator.toList == xs
  }

  property("IntAccumulatorDrain") = forAll(
    arbitrary[Seq[Int]],
    arbitrary[Seq[Int]]
  ) { (xs1, xs2) =>
    val acc1: IntAccumulator = Accumulator(xs1: _*)
    val acc2: IntAccumulator = Accumulator(xs2: _*)
    acc1.drain(acc2)
    acc1 sameElements Iterator.concat(xs1, xs2)
  }

  property("LongAccumulatorConvert") = forAll(
    arbitrary[Seq[Long]]
  ) { xs =>
    val acc: LongAccumulator = Accumulator(xs: _*)
    acc.toList == xs &&
      acc.stepper.iterator.toList == xs &&
      acc.toArray.toList == xs &&
      acc.to(List) == xs &&
      acc.iterator.toList == xs
  }

  property("LongAccumulatorDrain") = forAll(
    arbitrary[Seq[Long]],
    arbitrary[Seq[Long]]
  ) { (xs1, xs2) =>
    val acc1: LongAccumulator = Accumulator(xs1: _*)
    val acc2: LongAccumulator = Accumulator(xs2: _*)
    acc1.drain(acc2)
    acc1 sameElements Iterator.concat(xs1, xs2)
  }

  property("DoubleAccumulatorConvert") = forAll(
    arbitrary[Seq[Double]]
  ) { xs =>
    val acc: DoubleAccumulator = Accumulator(xs: _*)
    acc.toList == xs &&
      acc.stepper.iterator.toList == xs &&
      acc.toArray.toList == xs &&
      acc.to(List) == xs &&
      acc.iterator.toList == xs
  }

  property("DoubleAccumulatorDrain") = forAll(
    arbitrary[Seq[Double]],
    arbitrary[Seq[Double]]
  ) { (xs1, xs2) =>
    val acc1: DoubleAccumulator = Accumulator(xs1: _*)
    val acc2: DoubleAccumulator = Accumulator(xs2: _*)
    acc1.drain(acc2)
    acc1 sameElements Iterator.concat(xs1, xs2)
  }

  property("AnyAccumulatorConvert") = forAll(
    arbitrary[Seq[String]]
  ) { xs =>
    val acc: AnyAccumulator[String] = Accumulator(xs: _*)
    acc.toList == xs &&
      acc.stepper.iterator.toList == xs &&
      acc.toArray.toList == xs &&
      acc.to(List) == xs &&
      acc.iterator.toList == xs
  }

  property("AnyAccumulatorDrain") = forAll(
    arbitrary[Seq[String]],
    arbitrary[Seq[String]]
  ) { (xs1, xs2) =>
    val acc1: AnyAccumulator[String] = Accumulator(xs1: _*)
    val acc2: AnyAccumulator[String] = Accumulator(xs2: _*)
    acc1.drain(acc2)
    acc1 sameElements Iterator.concat(xs1, xs2)
  }

}
