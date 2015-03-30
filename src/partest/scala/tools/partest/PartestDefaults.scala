package scala.tools
package partest

import scala.concurrent.duration.Duration
import scala.tools.nsc.Properties.{ propOrNone => prop }
import java.lang.Runtime.{ getRuntime => runtime }

object PartestDefaults {
  def sourcePath  = prop("partest.srcdir")      getOrElse "files"
  def javaCmd     = prop("partest.javacmd")     getOrElse "java"
  def javacCmd    = prop("partest.javac_cmd")   getOrElse "javac"
  def javaOpts    = prop("partest.java_opts")   getOrElse  ""     // opts when running java during tests
  def scalacOpts  = prop("partest.scalac_opts") getOrElse  ""

  def testBuild   = prop("partest.build")
  def errorCount  = prop("partest.errors")  map (_.toInt) getOrElse 0
  def numThreads  = prop("partest.threads") map (_.toInt) getOrElse runtime.availableProcessors
  def waitTime    = Duration(prop("partest.timeout") getOrElse "4 hours")

  //def timeout     = "1200000"   // per-test timeout
}
