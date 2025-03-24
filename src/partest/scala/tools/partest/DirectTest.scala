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

import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.sys.process.{Parser => CommandLineParser}
import scala.tools.nsc._
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.tools.nsc.settings.ScalaVersion
import scala.util.chaining._

/** Test with code which is embedded as a string.
 *
 *  `DirectTest` allows for more complete control over settings, compiler
 *  configuration, sequence of events, etc. than does partest alone.
 *
 *  Tests must define `code` and `show()`. Minimally:
 *  ```
 *  def show() = assert(compile())
 *  ```
 *
 *  There are helper methods for creating settings and
 *  invoking a (newly constructed) compiler.
 */
abstract class DirectTest {
  // The program being tested in some fashion
  def code: String
  // produce the output to be compared against a checkfile
  def show(): Unit

  // the test file or dir, and output directory
  def testPath   = SFile(sys.props("partest.test-path"))
  def testOutput = Directory(sys.props("partest.output"))

  protected def pathOf(locations: String*) = locations.mkString(sys.props("path.separator"))

  // convenient for test classes not in a subpackage of scala
  final protected def tokenize(line: String): List[String] = CommandLineParser.tokenize(line)

  // override to add additional settings besides -d testOutput.path
  // default is -usejavacp
  def extraSettings: String = "-usejavacp"
  // a default Settings object using only extraSettings
  def settings: Settings = newSettings(tokenize(extraSettings))
  // settings factory using given args and also debug settings
  def newSettings(args: List[String]): Settings = newBaseSettings().tap { s =>
    val allArgs = debugSettings.pipe(db => if (db.isEmpty) args else args ++ tokenize(db))
    log(s"newSettings: allArgs = $allArgs")
    val (success, residual) = s.processArguments(allArgs, processAll = false)
    assert(success && residual.isEmpty, s"Bad settings [${args.mkString(",")}], residual [${residual.mkString(",")}]")
  }
  // scaladoc has custom settings
  def newBaseSettings(): Settings = new Settings

  // new compiler using given ad hoc args, -d and extraSettings
  def newCompiler(args: String*): Global = {
    val settings = newSettings(tokenize(s"""-d "${testOutput.path}" ${extraSettings}""") ++ args.toList)
    newCompiler(settings)
  }

  // compiler factory
  def newCompiler(settings: Settings): Global = Global(settings, reporter(settings))

  // reporter factory, console by default
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
  def main(args: Array[String]): Unit =
    try show()
    catch {
      case t: Exception =>
        println(t.getMessage)
        t.printStackTrace()
        sys.exit(1)
    }

  /**
    * Run a test only if the current java version is at least the version specified.
    */
  def testUnderJavaAtLeast[A](version: String)(yesRun: => A) = ifJavaAtLeast(version)(yesRun)
}

class TestUnderJavaAtLeast[A](version: String, yesRun: => A) {
  //val javaVersion = System.getProperty("java.specification.version")
  val javaVersion = scala.util.Properties.javaSpecVersion

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
  def otherwise(fallback: => A): A = {
    logInfo()
    if (shouldRun) yesRun else fallback
  }
}
