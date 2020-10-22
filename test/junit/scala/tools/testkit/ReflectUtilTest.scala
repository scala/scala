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

package scala.tools.testkit

import org.junit.Assert.assertEquals
import org.junit.Test
import AssertUtil._

class ReflectUtilTest {
  import ReflectUtil._

  class C {
    def f(x: String = "empty"): Unit = ()
    def f_=(x: String): Unit = ()
  }

  @Test def `find f`(): Unit = {
    assertEquals("f", getMethodAccessible[C]("f").getName)
    assertEquals("f_$eq", getMethodAccessible[C]("f_$eq").getName)
    assertEquals("f$default$1", getMethodAccessible[C]("f$default").getName)
  }
  @Test def `do not find partial default`(): Unit = assertFails(_ == "Missing method f$def") {
    getMethodAccessible[C]("f$def")
  }
  @Test def `do not find g`(): Unit = assertFails(_ == "Missing method g") {
    getMethodAccessible[C]("g")
  }
}
