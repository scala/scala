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

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class TargetTest {

  @Test def testSettingTargetSetting(): Unit = {
    def check(in: String, expect: String) = {
      val settings = new Settings
      settings.processArgumentString(in)
      Assert.assertEquals(expect, settings.target.value)
    }
    def checkFail(in: String) = {
      val settings = new Settings
      val (ok, _) = settings.processArgumentString(in)
      Assert.assertFalse(ok)
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

    checkFail("-target:jvm-13") // not yet...
    checkFail("-target:msil") // really?

  }

}
