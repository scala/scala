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
package nest

import java.io.{BufferedReader, FileWriter, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{CodeAction, NoPosition, Position, ScalaClassLoader}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.util.chaining._
import scala.sys.process._

object ExtConsoleReporter {
  def apply(settings: Settings, writer: PrintWriter) = {
    val loader = new ClassLoader(getClass.getClassLoader) with ScalaClassLoader
    loader.create[ConsoleReporter](settings.reporter.value, settings.errorFn)(settings, Console.in, writer, writer).tap(_.shortname = true)
  }
}
class PlainReporter(settings: Settings, reader: BufferedReader, writer: PrintWriter, echo: PrintWriter) extends ConsoleReporter(settings, reader, writer, echo) {
  override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit = writer.println(s"[$severity] [$pos]: $msg")
}

class TestSettings(cp: String, error: String => Unit) extends Settings(error) {
  @deprecated("Use primary constructor", "1.0.12")
  def this(cp: String) = this(cp, _ => ())
  nowarnings.value = false
  encoding.value   = "UTF-8"
  classpath.value  = cp
  //lint.add("_")
}

class PartestGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  // override def abort(msg: String): Nothing
  // override def globalError(msg: String): Unit
  // override def supplementErrorMessage(msg: String): String
}
class DirectCompiler(val runner: Runner) {
  def newGlobal(settings: Settings, reporter: Reporter): PartestGlobal =
    new PartestGlobal(settings, reporter)

  def newGlobal(settings: Settings, logWriter: FileWriter): Global =
    newGlobal(settings, ExtConsoleReporter(settings, new PrintWriter(logWriter, true)))


  /** Massage args to merge plugins and fix paths.
   *  Plugin path can be relative to test root, or cwd (".") means use output dir and copy scalac-plugin.xml there.
   *  Mix in the baseline options from the suiteRunner (scalacOpts, scalacExtraArgs).
   */
  private def updatePluginPath(args: List[String], out: AbstractFile, srcdir: AbstractFile): Seq[String] = {
    val dir = runner.suiteRunner.pathSettings.testRoot
    // The given path, or the output dir if ".", or a temp dir if output is virtual (since plugin loading doesn't like virtual)
    def pathOrCwd(p: String) =
      if (p == ".") {
        val plugxml = "scalac-plugin.xml"
        val pout = if (out.isVirtual) Directory.makeTemp() else Path(out.path)
        val srcpath = Path(srcdir.path)
        val pd = (srcpath / plugxml).toFile
        if (pd.exists) pd copyTo (pout / plugxml)
        pout.toAbsolute
      } else Path(p)
    def absolutize(path: String) = pathOrCwd(path) match {
      case x if x.isAbsolute => x.path
      case x                 => (dir / x).toAbsolute.path
    }

    val xprefix = "-Xplugin:"
    val (xplugs, others) = args.partition(_.startsWith(xprefix))
    val Xplugin =
      if (xplugs.isEmpty) Nil
      else List(xprefix + xplugs.map(_ stripPrefix xprefix).flatMap(_ split pathSeparator).map(absolutize).mkString(pathSeparator))

    // tests control most compiler settings, but let the environment tell us whether to exercise optimizer
    val filteredOpts = runner.suiteRunner.scalacOpts.split(' ').filter(_.startsWith("-opt"))

    runner.suiteRunner.scalacExtraArgs ++ filteredOpts ++ others ++ Xplugin
  }
  private def updatePluginPath(args: List[String]): Seq[String] = {
    import runner.testInfo.testFile
    val srcDir = if (testFile.isDirectory) testFile else Path(testFile).parent.jfile
    updatePluginPath(args, AbstractFile.getDirectory(runner.outDir), AbstractFile.getDirectory(srcDir))
  }

  def compile(opts0: List[String], sources: List[File]): TestState = {
    import runner.{sources => _, _}
    import testInfo._

    // add the instrumented library version to classpath -- must come first
    val specializedOverride: List[Path] =
      if (kind == "specialized")
        List(suiteRunner.pathSettings.srcSpecLib.fold(sys.error, identity))
      else Nil

    val classPath: List[Path] = specializedOverride ++ fileManager.testClassPath ++ List[Path](outDir)

    val parseArgErrors = ListBuffer.empty[String]

    val testSettings = new TestSettings(FileManager.joinPaths(classPath), s => parseArgErrors += s)
    val logWriter    = new FileWriter(logFile)
    val opts         = updatePluginPath(opts0)
    val command      = new CompilerCommand(opts.toList, testSettings)
    val reporter     = ExtConsoleReporter(testSettings, new PrintWriter(logWriter, true))
    val global       = newGlobal(testSettings, reporter)
    def errorCount   = reporter.errorCount

    // usually, -d outDir, but don't override setting by the test
    if (!testSettings.outdir.isSetByUser)
      testSettings.outputDirs.setSingleOutput(outDir.getPath)

    def reportError(s: String): Unit = reporter.error(NoPosition, s)

    parseArgErrors.toList foreach reportError

    // check that option processing succeeded
    if (opts0.nonEmpty) {
      if (!command.ok) reportError(opts0.mkString("bad options: ", space, ""))
      if (command.files.nonEmpty) reportError(command.files.mkString("flags file may only contain compiler options, found: ", space, ""))
    }

    suiteRunner.verbose(sources.map(_.testIdent).mkString("% compiling ", space, if (suiteRunner.debug) s" -d $outDir" else ""))

    def execCompile() =
      if (command.shouldStopWithInfo) {
        logWriter append (command getInfoMessage global)
        runner genFail "compilation stopped with info"
      } else {
        new global.Run compile sources.map(_.getPath)
        val result =
          if (!reporter.hasErrors) runner.genPass()
          else {
            reporter.finish()
            runner.genFail(s"compilation failed with $errorCount errors")
          }
        reporter.close()
        result
      }

    def execOtherCompiler() = {
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val logger = ProcessLogger(stdout append _, stderr append _)
      val resultCode = (suiteRunner.config.optCompilerPath.get + " " + sources.map(_.getPath).mkString(" ")) ! logger
      logWriter append stdout
      logWriter append stderr
      if (resultCode == 0) runner.genPass()
      else runner.genFail(s"compilation failed")
    }

    try
      if (suiteRunner.config.optCompilerPath.isEmpty) execCompile()
      else execOtherCompiler()
    catch runner.crashHandler
    finally logWriter.close()
  }
}
