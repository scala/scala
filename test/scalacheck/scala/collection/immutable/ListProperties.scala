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

import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{IterableFactory, View, mutable}

object ListProperties extends Properties("immutable.List") {

  val iterableGen: Gen[collection.Iterable[Int]] =
    for {
      data <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      factory <- Gen.oneOf[IterableFactory[collection.Iterable]](
        List, Vector, ArrayBuffer, mutable.ArrayDeque, Queue, ListBuffer, View
      )
    } yield factory.from(data)


  val iterableOnceGen: Gen[() => IterableOnce[Int]] =
    Gen.oneOf(iterableGen.map(it => () => it), iterableGen.map(it => () => it.iterator))

  property("list1 ::: list2 == list1.toVector.prependedAll(list2)") = forAll { (list1: List[Int], list2: List[Int]) =>
    (list1.prependedAll(list2): Seq[Int]) ?= list1.toVector.prependedAll(list2)
  }
  property("list1.prependedAll(iterableOnce) == list1.prependedAll(iterableOnce)") =
    forAll(arbitrary[List[Int]], iterableOnceGen){ (list1, it) =>
      (list1.prependedAll(it()): Seq[Int]) ?= list1.toVector.prependedAll(it())
  }

  property("List.from(iterableOnce) == Vector.from(iterableOnce)") =
    forAll(iterableOnceGen) { it =>
      (List.from(it()): Seq[Int]) ?= Vector.from(it())
    }
}
