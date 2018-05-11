/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.partest

import scala.tools.nsc._
import settings.ScalaVersion
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }
import reporters.{Reporter, ConsoleReporter}
import scala.tools.cmd.CommandLineParser

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
  def testPath   = SFile(sys.props("partest.test-path"))
  def testOutput = Directory(sys.props("partest.output"))

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

  def newCompiler(settings: Settings): Global = Global(settings, reporter(settings))

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

  /**
   * Run a test only if the current java version is at least the version specified.
   */
  def testUnderJavaAtLeast[A](version: String)(yesRun: =>A) = new TestUnderJavaAtLeast(version, { yesRun })

  class TestUnderJavaAtLeast[A](version: String, yesRun: => A) {
    val javaVersion = System.getProperty("java.specification.version")

    // the "ScalaVersion" class parses Java specification versions just fine
    val requiredJavaVersion = ScalaVersion(version)
    val executingJavaVersion = ScalaVersion(javaVersion)
    val shouldRun = executingJavaVersion >= requiredJavaVersion
    val preamble = if (shouldRun) "Attempting" else "Doing fallback for"

    def logInfo() = log(s"$preamble java $version specific test under java version $javaVersion")
 
   /*
    * If the current java version is at least 'version' then 'yesRun' is evaluated
    * otherwise 'fallback' is 
    */
    def otherwise(fallback: =>A): A = {
      logInfo()
      if (shouldRun) yesRun else fallback
    }
  }
}
