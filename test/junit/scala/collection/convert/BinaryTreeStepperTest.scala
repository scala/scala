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

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class BinaryTreeStepperTest {
  @Test
  def testStepper(): Unit = {
    val s = collection.immutable.TreeSet.from(Array(-1, 48, 111, 115, 0, 116, 5399)).stepper
    val t = collection.mutable.Set.empty[Int]
    while (s.hasStep) t.add(s.nextStep())

    assertEquals(Set(-1, 48, 111, 115, 0, 116, 5399), t)
  }
}
