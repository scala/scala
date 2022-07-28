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
import scala.util.Properties.isJavaAtLeast
import scala.util.Try

@RunWith(classOf[JUnit4])
class TargetTest {

  @Test def testSettingTargetSetting(): Unit = {
    def goodVersion(v: String): Boolean = Try(isJavaAtLeast(v)).getOrElse(false)
    def check(in: String, expect: String) =
      if (goodVersion(expect)) {
        val settings = new Settings(err => fail(s"Error output: $err"))
        val (ok, _) = settings.processArgumentString(in)
        assertTrue(ok)
        assertEquals(expect, settings.target.value)
      }
      else checkFail(in)
    def checkDeprecated(in: String, expect: String) = {
      val messages = ListBuffer.empty[String]
      val settings = new Settings(messages.append(_))
      val (ok, _) = settings.processArgumentString(in)
      assertTrue(messages.toString, ok)
      assertTrue(messages.isEmpty)
      assertEquals(expect, settings.target.value)
      assertTrue(s"Expected forcing: ${settings.target.deprecationMessage}", settings.target.deprecationMessage.exists(_.contains("is deprecated, forcing use of")))
    }
    def checkFail(in: String) = {
      val messages = ListBuffer.empty[String]
      val settings = new Settings(messages.append(_))
      val (ok, _) = settings.processArgumentString(in)
      assertFalse(ok)
      assertFalse(messages.isEmpty)
      assertEquals(2, messages.size)   // bad choice + bad option
      assertTrue(messages.exists(_.startsWith("bad option")))
    }

    checkDeprecated("-target:jvm-1.5", "8")
    checkDeprecated("-target:1.5", "8")

    checkDeprecated("-target:jvm-1.6", "8")
    checkDeprecated("-target:6", "8")

    checkDeprecated("-target:jvm-1.7", "8")
    checkDeprecated("-target:jvm-7", "8")

    check("-target:jvm-1.8", "8")
    check("-target:1.8", "8")
    check("-target:jvm-8", "8")
    check("-target:8", "8")

    check("-target:jvm-9", "9")
    check("-target:9", "9")
    checkFail("-target:jvm-1.9")
    checkFail("-target:1.9")

    check("-target:jvm-10", "10")
    check("-target:10", "10")

    check("-target:jvm-11", "11")
    check("-target:11", "11")

    check("-target:jvm-12", "12")
    check("-target:12", "12")

    // (scene missing)

    check("-target:jvm-16", "16")
    check("-target:16", "16")

    check("-target:jvm-17", "17")
    check("-target:17", "17")

    check("-target:jvm-18", "18")
    check("-target:18", "18")

    check("-target:jvm-19", "19")
    check("-target:19", "19")

    checkFail("-target:jvm-20")   // not yet...
    checkFail("-target:jvm-3000") // not in our lifetime
    checkFail("-target:msil")     // really?

  }

}
