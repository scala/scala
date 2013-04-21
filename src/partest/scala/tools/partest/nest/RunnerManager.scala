/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import java.io._
import java.net.URL
import scala.tools.nsc.Properties.{ jdkHome, javaHome, propOrElse }
import scala.util.Properties.{ envOrElse, isWin }
import scala.tools.nsc.{ Settings, CompilerCommand, Global }
import scala.tools.nsc.io.{ AbstractFile, PlainFile, Path, Directory, File => SFile }
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.{ ClassPath, FakePos, ScalaClassLoader, stackTraceString }
import ClassPath.{ join, split }
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.collection.{ mutable, immutable }
import scala.sys.process._
import java.util.concurrent.{ Executors, TimeUnit, TimeoutException }
import PartestDefaults.{ javaCmd, javacCmd }
import scala.tools.scalap.Main.decompileScala

class LogContext(val file: File, val writers: Option[(StringWriter, PrintWriter)])

object LogContext {
  def apply(file: File, swr: StringWriter, wr: PrintWriter): LogContext = {
    require (file != null)
    new LogContext(file, Some((swr, wr)))
  }
  def apply(file: File): LogContext = new LogContext(file, None)
}

object Output {
  object outRedirect extends Redirecter(out)
  object errRedirect extends Redirecter(err)

  System.setOut(outRedirect)
  System.setErr(errRedirect)

  import scala.util.DynamicVariable
  private def out = java.lang.System.out
  private def err = java.lang.System.err
  private val redirVar = new DynamicVariable[Option[PrintStream]](None)

  class Redirecter(stream: PrintStream) extends PrintStream(new OutputStream {
    def write(b: Int) = withStream(_ write b)

    private def withStream(f: PrintStream => Unit) = f(redirVar.value getOrElse stream)

    override def write(b: Array[Byte]) = withStream(_ write b)
    override def write(b: Array[Byte], off: Int, len: Int) = withStream(_.write(b, off, len))
    override def flush = withStream(_.flush)
    override def close = withStream(_.close)
  })

  // this supports thread-safe nested output redirects
  def withRedirected[T](newstream: PrintStream)(func: => T): T = {
    // note down old redirect destination
    // this may be None in which case outRedirect and errRedirect print to stdout and stderr
    val saved = redirVar.value
    // set new redirecter
    // this one will redirect both out and err to newstream
    redirVar.value = Some(newstream)

    try func
    finally {
      newstream.flush()
      redirVar.value = saved
    }
  }
}

class RunnerManager(kind: String, val fileManager: FileManager, params: TestRunParams) {
  import fileManager._

  fileManager.CLASSPATH += File.pathSeparator + PathSettings.scalaCheck
  fileManager.CLASSPATH += File.pathSeparator + PathSettings.diffUtils // needed to put diffutils on test/partest's classpath
  PathSettings.platformTools foreach (fileManager.CLASSPATH += File.pathSeparator + _)

  def runTest(testFile: File): TestState = {
    val runner = new Runner(testFile, fileManager) {
      override def testRunParams = params
    }
    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    if (fileManager.failed && !runner.logFile.canRead)
      runner.genPass()
    else {
      val (state, elapsed) = timed(runner.run())
      NestUI.reportTest(state)
      runner.cleanup()
      state
    }
  }
}
