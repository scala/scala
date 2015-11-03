package scala.tools
package partest

import scala.concurrent.duration.Duration
import scala.tools.nsc.Properties.{ propOrNone => prop }
import scala.util.Properties.jdkHome
import java.lang.Runtime.{ getRuntime => runtime }

object PartestDefaults {
  def sourcePath  = prop("partest.srcdir")      getOrElse "files"
  def javaCmd     = prop("partest.javacmd")     orElse    jdkexec("java")  getOrElse "java"
  def javacCmd    = prop("partest.javac_cmd")   orElse    jdkexec("javac") getOrElse "javac"
  def javaOpts    = prop("partest.java_opts")   getOrElse  ""     // opts when running java during tests
  def scalacOpts  = prop("partest.scalac_opts") getOrElse  ""

  def testBuild   = prop("partest.build")
  def errorCount  = prop("partest.errors")  map (_.toInt) getOrElse 0
  def numThreads  = prop("partest.threads") map (_.toInt) getOrElse runtime.availableProcessors
  def waitTime    = Duration(prop("partest.timeout") getOrElse "4 hours")

  //def timeout     = "1200000"   // per-test timeout

  // probe for the named executable
  private def jdkexec(name: String): Option[String] = {
    import scala.reflect.io.Path, Path._
    Some(Path(jdkHome) / "bin") filter (_.isDirectory) flatMap { p =>
      val candidates = (p walkFilter { e => (e.name == name || e.name.startsWith(s"$name.")) && e.jfile.canExecute }).toList
      (candidates find (_.name == name) orElse candidates.headOption) map (_.path)
    }
  }
}
