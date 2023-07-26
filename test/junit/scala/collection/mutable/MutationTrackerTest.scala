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

import java.util.ConcurrentModificationException

import org.junit.Test

import scala.annotation.nowarn
import scala.tools.testkit.AssertUtil.assertThrows

class MutationTrackerTest {
  @nowarn("cat=w-flag-value-discard")
  @Test
  def checkedIterator(): Unit = {
    var mutationCount = 0
    def it = new MutationTracker.CheckedIterator(List(1, 2, 3).iterator, mutationCount)
    val it1 = it
    it1.toList // does not throw
    val it2 = it
    mutationCount += 1
    assertThrows[ConcurrentModificationException](it2.toList, _ contains "iteration")
  }
}
