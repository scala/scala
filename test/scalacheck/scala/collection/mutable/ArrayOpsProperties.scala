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

package scala.collection.mutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

object ArrayOpsProperties extends Properties("ArrayOps") {
  type E = Int

  private def showArray[A](array: Array[A]): String = array.mkString("Array(", ",", ")")

  property("left.copyToArray(right) == min(left.length, right.length)") = forAll{ (left: Array[E], right: Array[E]) =>
    val returned = left.copyToArray(right)
    s"left.length: ${left.length}, right.length: ${right.length}" |:
      (returned ?= math.min(left.length, right.length))
  }

  property("left.copyToArray(right, start) returns min of (left.length, right.length - start)") =
    forAll(Arbitrary.arbitrary[Array[E]], Arbitrary.arbitrary[Array[E]], Gen.chooseNum[Int](0, 10000)) {
      (left: Array[E], right: Array[E], start: Int) =>
        val returned = left.copyToArray(right, start)
        s"left.length: ${left.length}, right.length: ${right.length}" |:
          (returned ?= (left.length min (right.length - start).max(0)))
    }

  property("left.copyToArray(right, start, len) returns min of (left.length, right.length - start, len)") =
    forAll(Arbitrary.arbitrary[Array[E]], Arbitrary.arbitrary[Array[E]], Gen.chooseNum[Int](0, 10000), Gen.chooseNum[Int](0, 10000)) {
      (left: Array[E], right: Array[E], start: Int, len: Int) =>
        val returned = left.copyToArray(right, start, len)
        s"left.length: ${left.length}, right.length: ${right.length}" |:
          (returned ?= (left.length min (right.length - start).max(0) min len.max(0)))
    }


  property("left.copyToArray(right) copies min(left.length, right.length) to right") = forAll{ (left: Array[E], right: Array[E]) =>

    left.copyToArray(right)

    val prefix = left.take(left.length min right.length)

    s"left: ${showArray(left)}, right: ${showArray(right)}" |: (Prop(left.startsWith(prefix)) && right.startsWith(prefix))
  }

  property("left.copyToArray(right, start) copies min of (left.length, right.length - start) to right") =
    forAll(Arbitrary.arbitrary[Array[E]], Arbitrary.arbitrary[Array[E]], Gen.chooseNum[Int](0, 10000)) {
      (left: Array[E], right: Array[E], start: Int) =>
        left.copyToArray(right, start)

        val prefix = left.take(left.length min (right.length - start).max(0))

        s"left: ${showArray(left)}, right: ${showArray(right)}, prefix: ${showArray(prefix)}" |:
          (Prop(left.startsWith(prefix)) && right.drop(start).startsWith(prefix))
    }

  property("left.copyToArray(right, start, len) copies min of (left.length, right.length - start, len) to right") =
    forAll(
      Arbitrary.arbitrary[Array[E]],
      Arbitrary.arbitrary[Array[E]],
      Gen.chooseNum[Int](0, 10000),
      Gen.chooseNum[Int](0, 10000)) {
      (left: Array[E], right: Array[E], start: Int, len) =>
        left.copyToArray(right, start)

        val prefix = left.take(left.length min (right.length - start).max(0) min len.max(0))

        s"left: ${showArray(left)}, right: ${showArray(right)}, prefix: ${showArray(prefix)}" |:
          (Prop(left.startsWith(prefix)) && right.drop(start).startsWith(prefix))
    }

  property("left.copyToArray(right) does not modify left") = forAll{ (left: Array[E], right: Array[E]) =>
    val beforeSeq = left.toList
    left.copyToArray(right)
    val afterSeq = left.toList
    s"before: $beforeSeq, after: $afterSeq" |: (afterSeq ?= beforeSeq)
  }

  property("left.copyToArray(right, start) does not modify left") =
    forAll(Arbitrary.arbitrary[Array[E]], Arbitrary.arbitrary[Array[E]], Gen.chooseNum[Int](0, 10000)) {
      (left: Array[E], right: Array[E], start: Int) =>
        val beforeSeq = left.toList
        left.copyToArray(right, start)
        val afterSeq = left.toList
        s"before: $beforeSeq, after: $afterSeq" |: (afterSeq ?= beforeSeq)
    }
  property("left.copyToArray(right, start, len) does not modify left") =
    forAll(
      Arbitrary.arbitrary[Array[E]],
      Arbitrary.arbitrary[Array[E]],
      Gen.chooseNum[Int](0, 10000),
      Gen.chooseNum[Int](0, 10000)
    ) {
      (left: Array[E], right: Array[E], start: Int, len: Int) =>
        val beforeSeq = left.toList
        left.copyToArray(right, start, len)
        val afterSeq = left.toList
        s"before: $beforeSeq, after: $afterSeq" |: (afterSeq ?= beforeSeq)
    }

}

