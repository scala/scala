/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest
package nest

import utils.Properties._
import scala.tools.nsc.Properties.{ versionMsg, setProp }
import scala.tools.nsc.util.CommandLineParser
import scala.collection.{ mutable, immutable }
import PathSettings.srcDir
import TestKinds._
import scala.reflect.internal.util.Collections.distinctBy

class ConsoleRunner extends DirectRunner {
  import NestUI._
  import NestUI.color._

  // So we can ctrl-C a test run and still hear all
  // the buffered failure info.
  scala.sys addShutdownHook issueSummaryReport()

  var fileManager: ConsoleFileManager = _

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

  private var summarizing      = false
  private var elapsedMillis    = 0L
  private var expectedFailures = 0
  private def isSuccess = failedTests.size == expectedFailures

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
            comment("partest " + state.testFile)
            echo(state.transcriptString + "\n")
          }
        }

        def files_s = failed0.map(_.testFile).mkString(""" \""" + "\n  ")
        echo("# Failed test paths (this command will update checkfiles)")
        echo("test/partest --update-check \\\n  " + files_s + "\n")
      }

      echo(message)
      levyJudgment()
    }
  }

  private val unaryArgs = List(
    "--pack", "--all",
    "--terse", "--verbose", "--show-diff", "--show-log", "--self-test",
    "--failed", "--update-check", "--version", "--ansi", "--debug", "--help"
  ) ::: standardArgs

  private val binaryArgs = List(
    "--grep", "--srcpath", "--buildpath", "--classpath", "--timeout"
  )

  def main(argstr: String) {
    val parsed = CommandLineParser(argstr) withUnaryArgs unaryArgs withBinaryArgs binaryArgs

    if (parsed isSet "--debug") NestUI.setDebug()
    if (parsed isSet "--verbose") NestUI.setVerbose()
    if (parsed isSet "--terse") NestUI.setTerse()
    if (parsed isSet "--show-diff") NestUI.setDiffOnFail()

    // Early return on no args, version, or invalid args
    if (parsed isSet "--version") return echo(versionMsg)
    if ((argstr == "") || (parsed isSet "--help")) return NestUI.usage()

    val (individualTests, invalid) = parsed.residualArgs map (p => Path(p)) partition denotesTestPath
    if (invalid.nonEmpty) {
      if (isPartestVerbose)
        invalid foreach (p => echoWarning(s"Discarding invalid test path " + p))
      else if (!isPartestTerse)
        echoWarning(s"Discarding ${invalid.size} invalid test paths")
    }

    parsed get "--srcpath" foreach (x => setProp("partest.srcdir", x))
    parsed get "--timeout" foreach (x => setProp("partest.timeout", x))

    fileManager =
      if (parsed isSet "--buildpath") new ConsoleFileManager(parsed("--buildpath"))
      else if (parsed isSet "--classpath") new ConsoleFileManager(parsed("--classpath"), true)
      else if (parsed isSet "--pack") new ConsoleFileManager("build/pack")
      else new ConsoleFileManager  // auto detection, see ConsoleFileManager.findLatest

    fileManager.updateCheck = parsed isSet "--update-check"
    fileManager.failed      = parsed isSet "--failed"

    val partestTests = (
      if (parsed isSet "--self-test") TestKinds.testsForPartest
      else Nil
    )

    val grepExpr = parsed get "--grep" getOrElse ""

    // If --grep is given we suck in every file it matches.
    var grepMessage = ""
    val greppedTests = if (grepExpr == "") Nil else {
      val paths = grepFor(grepExpr)
      if (paths.isEmpty)
        echoWarning(s"grep string '$grepExpr' matched no tests.\n")

      paths.sortBy(_.toString)
    }

    val isRerun = parsed isSet "--failed"
    val rerunTests = if (isRerun) TestKinds.failedTests else Nil
    def miscTests = partestTests ++ individualTests ++ greppedTests ++ rerunTests

    val givenKinds = standardArgs filter parsed.isSet
    val kinds = (
      if (parsed isSet "--all") standardKinds
      else if (givenKinds.nonEmpty) givenKinds map (_ stripPrefix "--")
      else if (invalid.isEmpty && miscTests.isEmpty && !isRerun) standardKinds // If no kinds, --grep, or individual tests were given, assume --all
      else Nil
    )
    val kindsTests = kinds flatMap testsFor
    val dir =
      if (fileManager.testClasses.isDefined) fileManager.testClassesDir
      else fileManager.testBuildFile getOrElse {
        fileManager.latestCompFile.getParentFile.getParentFile.getAbsoluteFile
      }

    def testContributors = {
      List(
        if (partestTests.isEmpty) "" else "partest self-tests",
        if (rerunTests.isEmpty) "" else "previously failed tests",
        if (kindsTests.isEmpty) "" else s"${kinds.size} named test categories",
        if (greppedTests.isEmpty) "" else s"${greppedTests.size} tests matching '$grepExpr'",
        if (individualTests.isEmpty) "" else "specified tests"
      ) filterNot (_ == "") mkString ", "
    }

    def banner = {
      val vmBin  = javaHome + fileSeparator + "bin"
      val vmName = "%s (build %s, %s)".format(javaVmName, javaVmVersion, javaVmInfo)
      val vmOpts = fileManager.JAVA_OPTS

    s"""|Scala compiler classes in:  $dir
        |Scala version is:           $versionMsg
        |Scalac options are:         ${fileManager.SCALAC_OPTS mkString " "}
        |Java binaries in:           $vmBin
        |Java runtime is:            $vmName
        |Java options are:           $vmOpts
        |Source directory is:        $srcDir
        |Available processors:       ${Runtime.getRuntime().availableProcessors()}
        |Java Classpath:             ${sys.props("java.class.path")}
      """.stripMargin
    }

    chatty(banner)

    val allTests: List[Path] = distinctBy(miscTests ++ kindsTests)(_.toCanonical) sortBy (_.toString)
    val grouped              = (allTests groupBy kindOf).toList sortBy (x => standardKinds indexOf x._1)

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
        val results = runTestsForFiles(paths map (_.jfile), kind)
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
    System exit ( if (isSuccess) 0 else 1 )
  }
}
