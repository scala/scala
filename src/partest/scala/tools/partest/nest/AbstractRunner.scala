/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest
package nest

import utils.Properties._
import scala.tools.nsc.Properties.{ versionMsg, propOrFalse, setProp }
import scala.collection.{ mutable, immutable }
import TestKinds._
import scala.reflect.internal.util.Collections.distinctBy
import scala.tools.cmd.{ CommandLine, CommandLineParser, Instance }

abstract class AbstractRunner(argstr: String) extends {
  val parsed = RunnerSpec.creator(CommandLineParser tokenize argstr)
} with RunnerSpec with Instance {

  val suiteRunner: SuiteRunner

  import suiteRunner._
  import NestUI._
  import NestUI.color._

  private var totalTests  = 0
  private val passedTests = mutable.ListBuffer[TestState]()
  private val failedTests = mutable.ListBuffer[TestState]()

  def comment(s: String) = echo(magenta("# " + s))
  def levyJudgment() = {
    if (totalTests == 0) echoMixed("No tests to run.")
    else if (elapsedMillis == 0) echoMixed("Test Run ABORTED")
    else if (isSuccess) echoPassed("Test Run PASSED")
    else echoFailed("Test Run FAILED")
  }

  def passFailString(passed: Int, failed: Int, skipped: Int): String = {
    val total = passed + failed + skipped
    val isSuccess = failed == 0
    def p0 = s"$passed/$total"
    def p  = ( if (isSuccess) bold(green(p0)) else p0 ) + " passed"
    def f  = if (failed == 0) "" else bold(red("" + failed)) + " failed"
    def s  = if (skipped == 0) "" else bold(yellow("" + skipped)) + " skipped"

    oempty(p, f, s) mkString ", "
  }

  protected var partestCmd       = "test/partest"
  protected var summarizing      = false
  protected var printSummary     = true
  private var elapsedMillis    = 0L
  private var expectedFailures = 0
  protected def isSuccess = failedTests.size == expectedFailures

  def issueSummaryReport() {
    // Don't run twice
    if (!summarizing) {
      summarizing = true

      val passed0   = passedTests.toList
      val failed0   = failedTests.toList
      val passed    = passed0.size
      val failed    = failed0.size
      val skipped   = totalTests - (passed + failed)
      val passFail  = passFailString(passed, failed, skipped)
      val elapsed   = if (elapsedMillis > 0) " (elapsed time: " + elapsedString(elapsedMillis) + ")" else ""
      val message   = passFail + elapsed

      if (failed0.nonEmpty) {
        if (isPartestVerbose) {
          echo(bold(cyan("##### Transcripts from failed tests #####\n")))
          failed0 foreach { state =>
            comment(partestCmd + " " + state.testFile)
            echo(state.transcriptString + "\n")
          }
        }

        def files_s = failed0.map(_.testFile).mkString(""" \""" + "\n  ")
        echo("# Failed test paths (this command will update checkfiles)")
        echo(partestCmd + " --update-check \\\n  " + files_s + "\n")
      }

      if (printSummary) {
        echo(message)
        levyJudgment()
      }
    }
  }

  def run(): Unit = {
    if (optDebug || propOrFalse("partest.debug")) NestUI.setDebug()
    if (optVerbose)  NestUI.setVerbose()
    if (optTerse)    NestUI.setTerse()
    if (optShowDiff) NestUI.setDiffOnFail()
    if (optShowLog)  NestUI.setLogOnFail()

    // Early return on no args, version, or invalid args
    if (optVersion) return echo(versionMsg)
    if (optHelp) return NestUI.usage()

    val (individualTests, invalid) = parsed.residualArgs map (p => Path(p)) partition denotesTestPath
    if (invalid.nonEmpty) {
      if (isPartestVerbose)
        invalid foreach (p => echoWarning(s"Discarding invalid test path " + p))
      else if (!isPartestTerse)
        echoWarning(s"Discarding ${invalid.size} invalid test paths")
    }

    optTimeout foreach (x => setProp("partest.timeout", x))

    if (!isPartestTerse)
      NestUI echo banner

    val partestTests = (
      if (optSelfTest) TestKinds.testsForPartest
      else Nil
    )

    val grepExpr = optGrep getOrElse ""

    // If --grep is given we suck in every file it matches.
    // TODO: intersect results of grep with specified kinds, if any
    val greppedTests = if (grepExpr == "") Nil else {
      val paths = grepFor(grepExpr)
      if (paths.isEmpty)
        echoWarning(s"grep string '$grepExpr' matched no tests.\n")

      paths.sortBy(_.toString)
    }

    val isRerun = optFailed
    val rerunTests = if (isRerun) TestKinds.failedTests else Nil
    def miscTests = partestTests ++ individualTests ++ greppedTests ++ rerunTests

    val givenKinds = standardKinds filter parsed.isSet
    val kinds = (
      if (givenKinds.nonEmpty) givenKinds
      else if (miscTests.isEmpty) standardKinds // If no kinds, --grep, or individual tests were given, assume --all
      else Nil
    )
    val kindsTests = kinds flatMap testsFor

    def testContributors = {
      List(
        if (partestTests.isEmpty) "" else "partest self-tests",
        if (rerunTests.isEmpty) "" else "previously failed tests",
        if (kindsTests.isEmpty) "" else s"${kinds.size} named test categories",
        if (greppedTests.isEmpty) "" else s"${greppedTests.size} tests matching '$grepExpr'",
        if (individualTests.isEmpty) "" else "specified tests"
      ) filterNot (_ == "") mkString ", "
    }

    val allTests: Array[Path] = distinctBy(miscTests ++ kindsTests)(_.toCanonical) sortBy (_.toString) toArray
    val grouped = (allTests groupBy kindOf).toArray sortBy (x => standardKinds indexOf x._1)

    totalTests = allTests.size
    expectedFailures = propOrNone("partest.errors") match {
      case Some(num)  => num.toInt
      case _          => 0
    }
    val expectedFailureMessage = if (expectedFailures == 0) "" else s" (expecting $expectedFailures to fail)"
    echo(s"Selected $totalTests tests drawn from $testContributors$expectedFailureMessage\n")

    val (_, millis) = timed {
      for ((kind, paths) <- grouped) {
        val num = paths.size
        val ss = if (num == 1) "" else "s"
        comment(s"starting $num test$ss in $kind")
        val results = runTestsForFiles(paths map (_.jfile.getAbsoluteFile), kind)
        val (passed, failed) = results partition (_.isOk)

        passedTests ++= passed
        failedTests ++= failed
        if (failed.nonEmpty) {
          comment(passFailString(passed.size, failed.size, 0) + " in " + kind)
        }
        echo("")
      }
    }
    this.elapsedMillis = millis
    issueSummaryReport()
  }
}
