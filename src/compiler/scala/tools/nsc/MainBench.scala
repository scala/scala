/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.File
import File.pathSeparator

import scala.tools.nsc.interactive.{ RefinedBuildManager, SimpleBuildManager }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.reflect.internal.util.{ BatchSourceFile, FakePos } //{Position}
import Properties.{ versionString, copyrightString, residentPromptString, msilLibPath }
import scala.reflect.internal.util.Statistics

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
        theCompiler.settings.Ystatistics.value = true
        Statistics.enabled = true
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
