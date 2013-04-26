/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc._
import io.Directory
import util.{ SourceFile, BatchSourceFile, CommandLineParser }
import reporters.{Reporter, ConsoleReporter}

/** A class for testing code which is embedded as a string.
 *  It allows for more complete control over settings, compiler
 *  configuration, sequence of events, etc. than does partest.
 */
abstract class DirectTest extends App {
  // The program being tested in some fashion
  def code: String
  // produce the output to be compared against a checkfile
  def show(): Unit

  // the test file or dir, and output directory
  def testPath   = io.File(sys.props("partest.test-path"))
  def testOutput = io.Directory(sys.props("partest.output"))

  // override to add additional settings with strings
  def extraSettings: String = ""
  // a default Settings object
  def settings: Settings = newSettings(CommandLineParser tokenize extraSettings)
  // a custom Settings object
  def newSettings(args: List[String]) = {
    val s = new Settings
    val allArgs = args ++ (CommandLineParser tokenize debugSettings)
    log("newSettings: allArgs = " + allArgs)
    s processArguments (allArgs, true)
    s
  }
  // new compiler
  def newCompiler(args: String*): Global = {
    val settings = newSettings((CommandLineParser tokenize ("-d \"" + testOutput.path + "\" " + extraSettings)) ++ args.toList)
    newCompiler(settings)
  }

  def newCompiler(settings: Settings): Global = {
    if (settings.Yrangepos.value) new Global(settings, reporter(settings)) with interactive.RangePositions
    else new Global(settings, reporter(settings))
  }

  def reporter(settings: Settings): Reporter = new ConsoleReporter(settings)

  private def newSourcesWithExtension(ext: String)(codes: String*): List[BatchSourceFile] =
    codes.toList.zipWithIndex map {
      case (src, idx) => new BatchSourceFile(s"newSource${idx + 1}.$ext", src)
    }

  def newJavaSources(codes: String*) = newSourcesWithExtension("java")(codes: _*)
  def newSources(codes: String*)     = newSourcesWithExtension("scala")(codes: _*)

  def compileString(global: Global)(sourceCode: String): Boolean = {
    withRun(global)(_ compileSources newSources(sourceCode))
    !global.reporter.hasErrors
  }

  def javaCompilationUnits(global: Global)(sourceCodes: String*) = {
    sourceFilesToCompiledUnits(global)(newJavaSources(sourceCodes: _*))
  }

  def sourceFilesToCompiledUnits(global: Global)(files: List[SourceFile]) = {
    withRun(global) { run =>
      run compileSources files
      run.units.toList
    }
  }

  def compilationUnits(global: Global)(sourceCodes: String*): List[global.CompilationUnit] = {
    val units = sourceFilesToCompiledUnits(global)(newSources(sourceCodes: _*))
    if (global.reporter.hasErrors) {
      global.reporter.flush()
      sys.error("Compilation failure.")
    }
    units
  }

  def withRun[T](global: Global)(f: global.Run => T): T = {
    global.reporter.reset()
    f(new global.Run)
  }

  // compile the code, optionally first adding to the settings
  def compile(args: String*) = compileString(newCompiler(args: _*))(code)

  /**  Constructor/main body  **/
  try show()
  catch { case t: Exception => println(t.getMessage) ; t.printStackTrace ; sys.exit(1) }

  /** Debugger interest only below this line **/
  protected def isDebug       = (sys.props contains "partest.debug") || (sys.env contains "PARTEST_DEBUG")
  protected def debugSettings = sys.props.getOrElse("partest.debug.settings", "")

  final def log(msg: => Any) {
    if (isDebug) Console.err println msg
  }
}
