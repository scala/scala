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

@RunWith(classOf[JUnit4])
class TargetTest {

  @Test def testSettingTargetSetting(): Unit = {
    def check(in: String, expect: String) = {
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
      assertTrue(messages.nonEmpty)
      assertEquals(2, messages.size)   // bad choice + bad option
      assertTrue(messages.exists(_.startsWith("bad option")))
    }

    check("-target:jvm-1.8", "8")
    check("-target:1.8", "8")
    check("-target:jvm-8", "8")
    check("-target:8", "8")

    check("-target:jvm-9", "9")
    check("-target:9", "9")
    // it's not Java 1.9, you reprobates!

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

    checkFail("-target:jvm-6")    // no longer
    checkFail("-target:jvm-18")   // not yet...
    checkFail("-target:jvm-3000") // not in our lifetime
    checkFail("-target:msil")     // really?

  }

}
