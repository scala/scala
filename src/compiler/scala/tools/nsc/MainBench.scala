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

package scala.tools.nsc

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object MainBench extends Driver with EvalLoop {

  lazy val theCompiler = Global(settings, reporter)

  override def newCompiler() = theCompiler

  val NIter = 50
  val NBest = 10

  override def main(args: Array[String]) = {
    val times = new Array[Long](NIter)
    var start = System.nanoTime()
    for (i <- 0 until NIter) {
      if (i == NIter-1) {
        theCompiler.settings.Ystatistics.value           = List("all")
        theCompiler.settings.YhotStatisticsEnabled.value = true
      }
      process(args)
      val end = System.nanoTime()
      val duration = (end-start)/1000000
      println(s"${duration}ms")
      times(i) = duration
      start = end
    }
    val avg = times.sorted.take(NBest).sum / NBest
    println(s"avg shortest $NBest times ${avg}ms")
  }
}
