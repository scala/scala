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


import org.scalacheck._
import Prop._

import scala.collection.{IterableFactory, View, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object QueueProperties extends Properties("immutable.Queue") {

  val iterableGen: Gen[collection.Iterable[Int]] =
    for {
      data <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      factory <-
      Gen.oneOf[IterableFactory[collection.Iterable]](
        List, Vector, ArrayBuffer, mutable.ArrayDeque, Queue, ListBuffer, View
      )
    } yield factory.from(data)

  property("(Queue.from(list1) ++ list2).apply(i) == (list1 ++ list2).apply(i)") = {

    val testCases = for {
      list1 <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      list2 <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      length = list1.length + list2.length
      if length > 0
      i <- Gen.choose(0, length - 1)
    } yield (list1, list2, i)

    // we do not shrink because shrinking will result in empty lists which will result in exceptions thrown in apply
    forAllNoShrink(testCases) { case (list1, list2, i) =>
      val left = Queue.from(list1).concat(list2)
      left.apply(i) ?= list1.concat(list2).apply(i)
    }
  }
  property("queue.concat(iterable) == list.concat(iterable)") = {
    val testCases = for {
      list1 <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      list2 <- Gen.listOf(Arbitrary.arbInt.arbitrary)
      iterable: collection.Iterable[Int] <- iterableGen
    } yield (list1, list2, iterable)

    forAll(testCases) { case (list1, list2, iterable) =>
      val result = list1.concat(list2).appendedAll(iterable)
      ((Queue.from(list1).concat(list2).concat(iterable): Seq[Int]) ?= result).label("concat") &&
        ((Queue.from(list1).appendedAll(list2).appendedAll(iterable): Seq[Int]) ?= result).label("appendedAll") &&
        ((Queue.from(list1).enqueueAll(list2).enqueueAll(iterable): Seq[Int]) ?= result).label("enqueueAll")
    }
  }

  property("Queue.from(iterable) == List.from(iterable)") = {
    forAll(iterableGen) { it =>
      (Queue.from(it): Seq[Int]) ?= List.from(it)
    }
  }
}
