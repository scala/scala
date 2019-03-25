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

package scala.collection.convert

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class AccumulatorTest {
  @Test
  def map(): Unit = {
    val sa = Accumulator("a", "b")
    val ia = Accumulator(1, 2)

    assert(Accumulator("a-a", "b-b").iterator sameElements  (sa.map(x => s"$x-$x"): AnyAccumulator[String]))
    assert(Accumulator("1", "2").iterator sameElements (ia.map(_.toString): AnyAccumulator[String]))
    assert(AnyAccumulator(2, 3).iterator sameElements (ia.map(_ + 1): AnyAccumulator[Int]))
  }
}
