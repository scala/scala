package scala.tools
package partest

import scala.concurrent.duration.Duration
import scala.tools.nsc.Properties.{ propOrElse, propOrNone, propOrEmpty }
import java.lang.Runtime.{ getRuntime => runtime }

object PartestDefaults {

  def testRootName  = propOrNone("partest.root")
  def srcDirName    = propOrElse("partest.srcdir", "files")
  def testRootDir   = testRootName map (x => Directory(x))

  // def classPath   = propOrElse("partest.classpath", "")
  def classPath   = PathResolver.Environment.javaUserClassPath    // XXX

  def javaCmd     = propOrElse("partest.javacmd", "java")
  def javacCmd    = propOrElse("partest.javac_cmd", "javac")
  def javaOpts    = propOrElse("partest.java_opts", "")
  def scalacOpts  = propOrElse("partest.scalac_opts", "")

  def testBuild  = propOrNone("partest.build")
  def errorCount = propOrElse("partest.errors", "0").toInt
  def numThreads = propOrNone("partest.threads") map (_.toInt) getOrElse runtime.availableProcessors
  def waitTime   = propOrNone("partest.timeout") map (Duration.apply) getOrElse Duration("4 hours")

  //def timeout     = "1200000"   // per-test timeout
}
