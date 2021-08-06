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

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

import scala.tools.testkit.AssertUtil.{assertThrown, assertThrows}

class AssertThrowsTest {
  class Foo extends Exception
  class SubFoo extends Foo
  class Bar extends Exception

  @Test
  def catchFoo(): Unit = assertThrows[Foo] { throw new Foo }

  @Test
  def catchSubclass(): Unit = assertThrows[Foo] { throw new SubFoo }

  @Test
  def wrongThrow(): Unit =
    assertTrue({
      try {
        assertThrows[Foo] { throw new Bar }
        false
      } catch {
        case b: Bar => fail("Bar shouldn't have been rethrown"); false
        case e: AssertionError => true
        case t: Throwable => fail(s"expected AssertionError but got $t"); false
      }
    }, "Wrong exception thrown")

  @Test
  def errorIfNoThrow(): Unit = {
    try {
      assertThrows[Foo] { () }
    } catch {
      case e: AssertionError => return
    }
    fail("assertThrows should error if the tested expression does not throw anything")
  }

  @Test
  def helpful(): Unit =
    try {
      assertThrown[Foo]((foo: Foo) => false)(throw new Foo)
    } catch {
      case ae: AssertionError =>
        assertEquals(1, ae.getSuppressed.length)
        assertEquals("Exception failed check: scala.tools.testkit.AssertThrowsTest$Foo", ae.getMessage)
        assertEquals(classOf[Foo], ae.getSuppressed.head.getClass)
      case t: Throwable => fail(s"Expected an AssertionError: $t")
    }

  @Test
  def discriminating(): Unit =
    try {
      assertThrown[Foo]((foo: Foo) => true)(throw new Bar)
    } catch {
      case ae: AssertionError =>
        assertEquals(1, ae.getSuppressed.length)
        assertEquals("Wrong exception: expected scala.tools.testkit.AssertThrowsTest$Foo but was scala.tools.testkit.AssertThrowsTest$Bar", ae.getMessage)
        assertEquals(classOf[Bar], ae.getSuppressed.head.getClass)
      case t: Throwable => fail(s"Expected an AssertionError: $t")
    }
}
