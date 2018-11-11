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

package scala.util.control

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._
import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
@deprecated("Test me, don't judge me", since = "forever")
class ControlThrowableTest {

  class MyCtl extends ControlThrowable

  @deprecated("Test me, don't use me", since = "forever")
  class LegacyCtl extends ControlThrowable("legacy", true)

  @Test
  def stackless(): Unit = assertThrown[MyCtl]((my: MyCtl) => my.getStackTrace.isEmpty)(throw new MyCtl)

  @Test
  def fatal(): Unit =
    new MyCtl match {
      case NonFatal(_) => fail("ControlThrowable was NonFatal")
      case _ => ()
    }

  @Test
  def nonsuppressant(): Unit =
    assertThrown((my: MyCtl) => my.getSuppressed.isEmpty) {
      val e = new MyCtl
      e.addSuppressed(new java.io.IOException)
      throw e
    }

  @Test
  def stackful(): Unit =
    assertThrown[LegacyCtl]((legacy: LegacyCtl) => !legacy.getStackTrace.isEmpty) {
      throw new LegacyCtl
    }
}
