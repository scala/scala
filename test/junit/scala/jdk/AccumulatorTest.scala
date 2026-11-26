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

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
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

  @Test
  def indexBounds(): Unit = {
    def check[A](acc: collection.mutable.Seq[A], elem: A): Unit = {
      val n = acc.length
      for (ix <- List(-1, n, n + 1)) {
        assertThrows[IndexOutOfBoundsException](acc(ix))
        assertThrows[IndexOutOfBoundsException](acc(ix) = elem)
      }
      assertEquals(n, acc.length)
    }
    // a single chunk with spare capacity, and several chunks
    for (n <- List(0, 3, 100)) {
      check(IntAccumulator.fromSpecific(1 to n), 7)
      check(LongAccumulator.fromSpecific((1 to n).map(_.toLong)), 7L)
      check(DoubleAccumulator.fromSpecific((1 to n).map(_.toDouble)), 7.0)
      check(AnyAccumulator.from((1 to n).map(_.toString)), "7")
    }
  }
}
