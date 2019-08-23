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

package scala.tools.partest
package nest

import java.io.{FileWriter, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.NoPosition
import scala.reflect.io.AbstractFile
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.sys.process._

object ExtConsoleReporter {
  def apply(settings: Settings, writer: PrintWriter) = {
    val r = new ConsoleReporter(settings, Console.in, writer, writer)
    r.shortname = true
    r
  }
}

class TestSettings(cp: String, error: String => Unit) extends Settings(error) {
  @deprecated("Use primary constructor", "1.0.12")
  def this(cp: String) = this(cp, _ => ())
  nowarnings.value  = false
  encoding.value    = "UTF-8"
  classpath.value   = cp
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
    *  Plugin path can be relative to test root, or cwd is out.
    *  While we're at it, mix in the baseline options, too.
    *  That's how ant passes in the plugins dir.
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

  def compile(opts0: List[String], sources: List[File]): TestState = {
    import runner.{sources => _, _}
    import testInfo._

    // adding codelib.jar to the classpath
    // codelib provides the possibility to override standard reify
    // this shields the massive amount of reification tests from changes in the API
    val codeLib = suiteRunner.pathSettings.srcCodeLib.fold[List[Path]](x => Nil, lib => List[Path](lib))
    // add the instrumented library version to classpath -- must come first
    val specializedOverride: List[Path] =
      if (kind == "specialized")
        List(suiteRunner.pathSettings.srcSpecLib.fold(sys.error, identity))
      else Nil

    val classPath: List[Path] = specializedOverride ++ codeLib ++ fileManager.testClassPath ++ List[Path](outDir)

    val parseArgErrors = ListBuffer.empty[String]

    val testSettings = new TestSettings(FileManager.joinPaths(classPath), s => parseArgErrors += s)
    val logWriter    = new FileWriter(logFile)
    val srcDir       = if (testFile.isDirectory) testFile else Path(testFile).parent.jfile
    val opts         = updatePluginPath(opts0, AbstractFile getDirectory outDir, AbstractFile getDirectory srcDir)
    val command      = new CompilerCommand(opts.toList, testSettings)
    val reporter     = ExtConsoleReporter(testSettings, new PrintWriter(logWriter, true))
    val global       = newGlobal(testSettings, reporter)
    def errorCount   = reporter.errorCount

    testSettings.outputDirs setSingleOutput outDir.getPath

    def reportError(s: String): Unit = reporter.error(NoPosition, s)

    parseArgErrors.toList foreach reportError

    // check that option processing succeeded
    if (opts0.nonEmpty) {
      if (!command.ok) reportError(opts0.mkString("bad options: ", space, ""))
      if (command.files.nonEmpty) reportError(command.files.mkString("flags file may only contain compiler options, found: ", space, ""))
    }

    suiteRunner.verbose(s"% compiling ${ sources.map(_.testIdent).mkString(space) }${ if (suiteRunner.debug) " -d " + outDir else ""}")

    def execCompile() =
      if (command.shouldStopWithInfo) {
        logWriter append (command getInfoMessage global)
        runner genFail "compilation stopped with info"
      } else {
        new global.Run compile sources.map(_.getPath)
        if (!reporter.hasErrors) runner.genPass()
        else {
          reporter.finish()
          reporter.close()
          runner.genFail(s"compilation failed with $errorCount errors")
        }
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

    try {
      if (suiteRunner.config.optCompilerPath.isEmpty) execCompile()
      else execOtherCompiler()
    }
    catch   { case t: Throwable => reportError(t.getMessage) ; runner.genCrash(t) }
    finally { logWriter.close() }
  }
}
