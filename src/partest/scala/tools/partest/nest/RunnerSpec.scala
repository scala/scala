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

package scala.tools.partest.nest

import language.postfixOps

trait RunnerSpec extends Spec with Meta.StdOpts with Interpolation {
  def referenceSpec       = RunnerSpec
  def programInfo         = Spec.Info(
      "console-runner",
      "Usage: ConsoleRunner [options] [test test ...]",
      "scala.tools.partest.nest.ConsoleRunner")

  heading("Test categories:")
  val optPos          = "pos"          / "run compilation tests (success)"   --?
  val optNeg          = "neg"          / "run compilation tests (failure)"   --?
  val optRun          = "run"          / "run REPL and backend tests" --?
  val optJvm          = "jvm"          / "run JVM backend tests"             --?
  val optRes          = "res"          / "run resident compiler tests"       --?
  val optScalap       = "scalap"       / "run scalap tests"                  --?
  val optSpecialized  = "specialized"  / "run specialization tests"          --?
  val optInstrumented = "instrumented" / "run instrumented tests"            --?
  val optPresentation = "presentation" / "run presentation compiler tests"   --?

  heading("Test runner options:")
  val optFailed       = "failed"       / "run only those tests that failed during the last run"                           --?
  val optTimeout      = "timeout"      / "aborts the test suite after the given amount of time"                           --|
  val optPack         = "pack"         / "pick compiler/reflect/library in build/pack, and run all tests"                 --?
  val optGrep         = "grep"         / "run all tests whose source file contains the expression given to grep"          --|
  val optUpdateCheck  = "update-check" / "instead of failing tests with output change, update checkfile (use with care!)" --?
  val optNoExec       = "no-exec"      / "instead of running tests, stop after dry-run compilation"                       --?
  val optBuildPath    = "buildpath"    / "set (relative) path to build jars (ex.: --buildpath build/pack)"                --|
  val optClassPath    = "classpath"    / "set (absolute) path to build classes"                                           --|
  val optSourcePath   = "srcpath"      / "set (relative) path to test source files (ex.: --srcpath pending)"              --|
  // Compile with a different compiler, not used internally. Used by AxLang, see PR #8407.
  // Note: this will be much slower than creating a new global in the same JVM.
  // Note: run tests will still use the internal Scala version for running.
  val optCompilerPath = "compilerpath" / "set (absolute) path to a different compiler to test (ex.: --compilerpath /usr/local/bin/scalac)" --|

  heading("Test output options:")
  val optShowDiff     = "show-diff"    / "show diffs for failed tests"                       --?
  val optShowLog      = "show-log"     / "show log files for failed tests"                   --?
  val optVerbose      = "verbose"      / "show verbose progress information"                 --?
  val optTerse        = "terse"        / "show terse progress information"                   --?
  val optDebug        = "debug"        / "enable debugging output, preserve generated files" --?

  heading("Other options:")
  val optDev     = "realeasy" / "real easy way to test --release 8 and check uncommitted checks" --?
  val optBranch  = "branch"   / "test changes on this branch"                                    --?
  val optVersion = "version"  / "show Scala version and exit"                                    --?
  val optHelp    = "help"     / "show this page and exit"                                        --?

}

object RunnerSpec extends RunnerSpec with Reference {
  trait Config extends RunnerSpec with Instance

  type ThisCommandLine = CommandLine
  def creator(args: List[String]): ThisCommandLine = new CommandLine(RunnerSpec, args)

  // TODO: restructure to avoid using early initializers
  def forArgs(args: Array[String]): Config = new { val parsed = creator(args.toList) } with Config
}
