/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

abstract class MemoryTest {
  def maxDelta: Double
  def calcsPerIter: Int
  def calc(): Unit

  def main(args: Array[String]): Unit = {
    val rt = Runtime.getRuntime()
    def memUsage() = {
      import java.lang.management._
      import scala.jdk.CollectionConverters._
      val pools = ManagementFactory.getMemoryPoolMXBeans.asScala
      pools.map(_.getUsage.getUsed).sum / 1000000d
    }

    val history = scala.collection.mutable.ListBuffer[Double]()
    def stressTestIter() = {
      var i = 0
      while (i < calcsPerIter) { calc(); i += 1 }
      1 to 5 foreach (_ => rt.gc())
      history += memUsage()
    }

    1 to 5 foreach (_ => stressTestIter())
    val reference = memUsage()
    1 to 5 foreach (_ => stressTestIter())
    1 to 5 foreach (_ => rt.gc())
    val result = memUsage()
    history += result

    val delta = result - reference
    if (delta > maxDelta) {
      println("FAILED")
      history foreach (mb => println(s"$mb Mb"))
    }
  }
}
