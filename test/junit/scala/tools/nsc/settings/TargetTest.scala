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

package scala.tools.nsc
package settings

import org.junit.{Assert, Test}, Assert.{assertEquals, assertFalse, assertTrue, fail}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.settings.StandardScalaSettings._
import scala.tools.testkit.AssertUtil.assertFails
import scala.util.Properties.isJavaAtLeast
import scala.util.Try

@RunWith(classOf[JUnit4])
class TargetTest {

  private def lately[A](body: => A): Unit = if (isJavaAtLeast(11)) body: Unit

  private def eightly[A](body: => A): Unit = if (!isJavaAtLeast(9)) body: Unit

  @Test def testSettingTargetSetting(): Unit = {
    def goodVersion(v: String): Boolean = Try(isJavaAtLeast(v)).getOrElse(false)
    def check(in: String, expect: String) = if (goodVersion(expect)) checkSuccess(in, expect) else checkFail(in)
    def checkSuccess(in: String, expect: String) = {
      val settings = new Settings(err => fail(s"Error output: $err"))
      val (ok, _) = settings.processArgumentString(in)
      assertTrue(ok)
      assertEquals(expect, settings.target.value)
    }
    def checkFail(in: String) = {
      val messages = ListBuffer.empty[String]
      val settings = new Settings(messages.addOne)
      val (ok, _) = settings.processArgumentString(in)
      assertFalse(ok)
      assertFalse(messages.isEmpty)
      assertEquals(2, messages.size)   // bad choice + bad option
      assertTrue(messages.exists(_.startsWith("bad option")))
    }

    check("-target:jvm-1.8", "8")
    check("-target:1.8", "8")
    check("-target:jvm-8", "8")
    check("-target:8", "8")

    check("-target:jvm-9", "9")
    check("-target:9", "9")
    checkFail("-target:1.9")      // it's not Java 1.9, you reprobates!
    checkFail("-target:jvm-1.9")

    (MinTargetVersion to MaxTargetVersion).map(_.toString).foreach { v =>
      check(s"-target:jvm-$v", v)
      check(s"-target:$v", v)
    }
    checkFail(s"-target:jvm-${MaxTargetVersion+1}")
    checkFail(s"-target:${MaxTargetVersion+1}")
    checkFail("-target:jvm-6")    // no longer
    checkFail("-target:jvm-7")    // no longer
    checkFail("-target:jvm-3000") // not in our lifetime
    checkFail("-target:msil")     // really?
  }
  @Test def `respect user target`: Unit = lately {
    val settings = new Settings(err => fail(s"Error output: $err"))
    val (ok, _) = settings.processArgumentString("--release:11 --target:8")
    assertTrue(ok)
    assertEquals("8", settings.target.value)
    assertEquals("11", settings.release.value)
    assertEquals("8", settings.targetValue)
    assertEquals(Some("11"), settings.releaseValue)
  }
  @Test def `target is release if specified`: Unit = lately {
    val settings = new Settings(err => fail(s"Error output: $err"))
    val (ok, _) = settings.processArgumentString("--release:11")
    assertTrue(ok)
    assertEquals("8", settings.target.value)        // default
    assertEquals(None, settings.target.valueSetByUser)
    assertEquals("11", settings.release.value)
    assertEquals("11", settings.targetValue)        // because --release:11
    assertEquals(Some("11"), settings.releaseValue)
  }
  @Test def `disrespect user target`: Unit = lately {
    val settings = new Settings(err => fail(s"Error output: $err"))
    assertFails(_.contains("-release cannot be less than -target")) {
      settings.processArgumentString("--release:8 --target:11")
    }
  }
  @Test def `cannot bump release on jdk 8`: Unit = eightly {
    val settings = new Settings(err => fail(s"Error output: $err"))
    assertFails(_.contains("'9' is not a valid choice for '-release'")) {
      settings.processArgumentString("--release:9")
    }
  }
}
