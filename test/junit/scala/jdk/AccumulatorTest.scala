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

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class AccumulatorTest {
  @Test
  def map(): Unit = {
    val sa = Accumulator("a", "b")
    val ia = Accumulator(1, 2)

    assertEquals(Accumulator("a-a", "b-b"), sa.map(x => s"$x-$x"): AnyAccumulator[String])
    assertEquals(Accumulator("1", "2"), ia.map(_.toString): AnyAccumulator[String])
    assertEquals(AnyAccumulator(2, 3), ia.map(_ + 1): IntAccumulator)
    assertTrue(ia.forall(_ > 0))
    assertEquals(1L, ia.filter(_ > 0).countLong(_ > 1))

    ia(1) = 33
    assertEquals(List(1,33), ia)
    sa(0) = "ua"
    assertEquals(List("ua", "b"), sa)
  }
}
