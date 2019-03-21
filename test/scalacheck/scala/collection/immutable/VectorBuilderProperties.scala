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

package scala.collection.immutable

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

object VectorBuilderProperties extends Properties("VectorBuilder") {

  type Elem = Int

  /** Wrapper for all the many parameters to our properties
    *
    * @param a first vector to add
    * @param dropA number of elems to drop from `a` before adding
    * @param dropRightA number of elems to dropRight from `a` before adding
    * @param b second vector to add
    * @param dropB number of elems to drop from `b` before adding
    * @param dropRightB number of elems to dropRight from `b` before adding
    * @param c first vector to add
    * @param dropC number of elems to drop from `c` before adding
    * @param dropRightC number of elems to dropRight from `c` before adding
    */
  case class AddAllTestCase(
    a: Vector[Elem],
    dropA: Int,
    dropRightA: Int,
    b: Vector[Elem],
    dropB: Int,
    dropRightB: Int,
    c: Vector[Elem],
    dropC: Int,
    dropRightC: Int
  )

  implicit val arbAddAllTestCase: Arbitrary[AddAllTestCase] =
    Arbitrary {
      for {
        a <- arbitrary[Vector[Elem]]
        dropA <- Gen.choose(0, 500)
        dropRightA <- Gen.choose(0, 500)
        b <- arbitrary[Vector[Elem]]
        dropB <- Gen.choose(0, 500)
        dropRightB <- Gen.choose(0, 500)
        c <- arbitrary[Vector[Elem]]
        dropC <- Gen.choose(0, 500)
        dropRightC <- Gen.choose(0, 500)
      } yield AddAllTestCase(a, dropA, dropRightA, b, dropB, dropRightB, c, dropC, dropRightC)
    }

  property("addAll(a).addAll(b).addAll(c).result() == a ++ b ++ c") =
    forAll { testCase: AddAllTestCase =>
      val AddAllTestCase(a, dropA, dropRightA, b, dropB, dropRightB, c, dropC, dropRightC) = testCase

      val actual: Seq[Elem] =
        new VectorBuilder[Elem]
          .addAll(a.drop(dropA).dropRight(dropRightA))
          .addAll(b.drop(dropB).dropRight(dropRightB))
          .addAll(c.drop(dropC).dropRight(dropRightC))
          .result()

      val expected =
        a.toList.drop(dropA).dropRight(dropRightA) ++
        b.toList.drop(dropB).dropRight(dropRightB) ++
        c.toList.drop(dropC).dropRight(dropRightC)

      actual ?= expected
    }
  property("addAll(a).addAll(b).addAll(c).size == (a ++ b ++ c).size") =
    forAll { testCase: AddAllTestCase =>
      val AddAllTestCase(a, dropA, dropRightA, b, dropB, dropRightB, c, dropC, dropRightC) = testCase

      val actual: Int =
        new VectorBuilder[Elem]
          .addAll(a.drop(dropA).dropRight(dropRightA))
          .addAll(b.drop(dropB).dropRight(dropRightB))
          .addAll(c.drop(dropC).dropRight(dropRightC))
          .size

      val expected =
        (a.toList.drop(dropA).dropRight(dropRightA) ++
          b.toList.drop(dropB).dropRight(dropRightB) ++
          c.toList.drop(dropC).dropRight(dropRightC)).length

      actual ?= expected
    }
}
