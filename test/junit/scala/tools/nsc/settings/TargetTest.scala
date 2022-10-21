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
import scala.util.Properties.isJavaAtLeast
import scala.util.Try

@RunWith(classOf[JUnit4])
class TargetTest {

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
}
