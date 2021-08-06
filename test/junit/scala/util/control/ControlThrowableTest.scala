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

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.AssertUtil.assertThrown

@deprecated("Test me, don't judge me", since = "forever")
class ControlThrowableTest {

  class MyCtl extends ControlThrowable

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
}
