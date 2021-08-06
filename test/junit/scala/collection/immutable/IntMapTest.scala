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

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.ReflectUtil

class IntMapTest {
  @Test
  def `isEmpty O(1)`(): Unit = {
    val m = IntMap(1 -> (), 2 -> (), 3 -> ())
    ReflectUtil.getFieldAccessible[IntMap.Bin[_]]("left").set(m, null)
    ReflectUtil.getFieldAccessible[IntMap.Bin[_]]("right").set(m, null)
    assertFalse(m.isEmpty) // no NPE, does not access left or right
  }
}
