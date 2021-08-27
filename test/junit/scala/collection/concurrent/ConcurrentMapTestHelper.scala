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

package scala.collection.concurrent

import scala.concurrent.duration.SECONDS

object ConcurrentMapTestHelper {
  def genericTest_filterInPlace(newMap: => Map[String, Int]): Unit = {
    val tester = new ConcurrentMapTester(newMap += "k1" -> 0 += "k2" -> 0)

    tester.runTasks(5, SECONDS)(
      _.filterInPlace((_, v) => {
        SECONDS.sleep(2)
        v > 0
      }),
      map => {
        SECONDS.sleep(1)
        map("k1") = 1
      },
    )

    tester.assertContainsEntry("k1", 1) // can get `0` if racy implementation
    tester.assertDoesNotContain("k2")
  }

  def genericTest_mapValuesInPlace(newMap: => Map[String, Int]): Unit = {
    val tester = new ConcurrentMapTester(newMap += "k" -> 0)
    tester.runTasks(5, SECONDS)(
      _.mapValuesInPlace((_, v) => {
        SECONDS.sleep(2)
        v + 5
      }),
      map => {
        SECONDS.sleep(1)
        map("k") = 1
      },
    )

    tester.assertExistsEntry("k", x => x == 1 || x == 6) // can get `5` if racy implementation
  }
}
