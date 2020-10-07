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

package scala.tools
package partest
package nest

import utils.Properties._
import scala.tools.nsc.Properties.{propOrFalse, setProp, versionMsg}
import scala.collection.mutable
import scala.reflect.internal.util.Collections.distinctBy
import scala.util.{Try, Success, Failure}
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.NANOSECONDS

class AbstractRunner(val config: RunnerSpec.Config, protected final val testSourcePath: String, val fileManager: FileManager) {

  val javaCmdPath: String            = PartestDefaults.javaCmd
  val javacCmdPath: String           = PartestDefaults.javacCmd
  val scalacExtraArgs: Seq[String]   = Seq.empty
  val javaOpts: String               = PartestDefaults.javaOpts
  val scalacOpts: String             = PartestDefaults.scalacOpts
  val debug: Boolean                 = config.optDebug || propOrFalse("partest.debug")
  val verbose: Boolean               = config.optVerbose
  val terse: Boolean                 = config.optTerse

  protected val printSummary         = true
  protected val partestCmd           = "test/partest"

  private[this] var totalTests       = 0
  private[this] val passedTests      = mutable.ListBuffer[TestState]()
  private[this] val failedTests      = mutable.ListBuffer[TestState]()

  private[this] var summarizing      = false
  private[this] var elapsedMillis    = 0L
  private[this] var expectedFailures = 0
  private[this] var onlyIndividualTests = false

  val pathSettings = new PathSettings(testSourcePath)

  private[this] val testKinds = new TestKinds(pathSettings)
  import testKinds._

  val log = new ConsoleLog(sys.props contains "partest.colors")
  import log._

  private[this] val testNum = new java.util.concurrent.atomic.AtomicInteger(1)
  @volatile private[this] var testNumberFmt = "%3d"
  private[this] def testNumber = testNumberFmt format testNum.getAndIncrement()
  def resetTestNumber(max: Int = -1): Unit = {
    testNum set 1
    val width = if (max > 0) max.toString.length else 3
    testNumberFmt = s"%${width}d"
  }

  private[this] val realSysErr = System.err

  def statusLine(state: TestState, durationMs: Long) = {
    import state._
    import TestState._
    val colorizer = state match {
      case _: Skip     => yellow
      case _: Updated  => cyan
      case s if s.isOk => green
      case _           => red
    }
    val word = bold(colorizer(state.shortStatus))
    def durationString = if (durationMs > PartestDefaults.printDurationThreshold) f"[duration ${(1.0 * durationMs) / 1000}%.2fs]" else ""
    f"$word $testNumber - $testIdent%-40s$reasonString$durationString"
  }

  def reportTest(state: TestState, info: TestInfo, durationMs: Long, diffOnFail: Boolean, logOnFail: Boolean): List[String] = {
    def errInfo: List[String] = {
      def showLog() =
        if (info.logFile.canRead) List (
          bold(cyan(s"##### Log file '${info.logFile}' from failed test #####\n")),
          info.logFile.fileContents
        ) else Nil
      val diffed = 
        if (diffOnFail) {
          val differ = bold(red("% ")) + "diff "
          state.transcript.find(_ startsWith differ) match {
            case Some(diff) => diff :: Nil
            case None if !logOnFail && !verbose => showLog()
            case _ => Nil
          }
        } else Nil
      val logged = if (logOnFail) showLog() else Nil
      diffed ::: logged
    }
    if (terse) {
      if (state.isOk) { printDot() ; Nil }
      else { printEx() ; statusLine(state, durationMs) :: errInfo }
    } else {
      echo(statusLine(state, durationMs))
      if (!state.isOk) errInfo.foreach(echo)
      Nil
    }
  }

  def verbose(msg: => String): Unit =
    if (verbose) realSysErr.println(msg)

  def showAllJVMInfo(): Unit = {
    verbose(vmArgString)
    verbose(allPropertiesString)
  }

  private[this] def comment(s: String) = echo(magenta("# " + s))

  private[this] def levyJudgment() = {
    if (totalTests == 0) echoMixed("No tests to run.")
    else if (elapsedMillis == 0) echoMixed("Test Run ABORTED")
    else if (isSuccess) echoPassed("Test Run PASSED")
    else echoFailed("Test Run FAILED")
  }

  private[this] def passFailString(passed: Int, failed: Int, skipped: Int): String = {
    val total = passed + failed + skipped
    val isSuccess = failed == 0
    def p0 = s"$passed/$total"
    def p  = ( if (isSuccess) bold(green(p0)) else p0 ) + " passed"
    def f  = if (failed == 0) "" else bold(red("" + failed)) + " failed"
    def s  = if (skipped == 0) "" else bold(yellow("" + skipped)) + " skipped"

    oempty(p, f, s) mkString ", "
  }

  private[this] def isSuccess = failedTests.size == expectedFailures

  def issueSummaryReport(): Unit = {
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
        if (verbose) {
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

  /** Run the tests and return the success status */
  def run(): Boolean = {
    setUncaughtHandler

    if (config.optVersion) echo(versionMsg)
    else if (config.optHelp) {
      echo(s"Usage: $partestCmd [options] [test test ...]")
      echo(RunnerSpec.helpMsg)
    }
    else {
      val norm = Function.chain(Seq(testIdentToTestPath, checkFileToTestFile, testFileToTestDir, testDirToTestFile))
      val (individualTests, invalid) = config.parsed.residualArgs map (p => norm(Path(p))) partition denotesTestPath
      if (invalid.nonEmpty) {
        if (verbose)
          invalid foreach (p => echoWarning(s"Discarding invalid test path " + p))
        else if (!terse)
          echoWarning(s"Discarding ${invalid.size} invalid test paths")
      }

      config.optTimeout foreach (x => setProp("partest.timeout", x))

      if (!terse)
        echo(banner)

      val grepExpr = config.optGrep getOrElse ""

      // If --grep is given we suck in every file it matches.
      // TODO: intersect results of grep with specified kinds, if any
      val greppedTests = if (grepExpr == "") Nil else {
        val paths = grepFor(grepExpr)
        if (paths.isEmpty)
          echoWarning(s"grep string '$grepExpr' matched no tests.\n")

        paths.sortBy(_.toString)
      }

      val isRerun = config.optFailed
      val rerunTests = if (isRerun) testKinds.failedTests else Nil
      def miscTests = individualTests ++ greppedTests ++ rerunTests

      val givenKinds = standardKinds filter config.parsed.isSet
      val kinds = (
        if (givenKinds.nonEmpty) givenKinds
        else if (miscTests.isEmpty && invalid.isEmpty) standardKinds // If no kinds, --grep, or individual tests were given, assume --all, unless there were invalid files specified
        else Nil
      )
      val kindsTests = kinds.flatMap { k =>
        val (good, bad) = testsFor(k)
        bad.foreach(baddie => echoWarning(s"Extraneous file: $baddie"))
        good
      }

      def testContributors = {
        List(
          if (rerunTests.isEmpty) "" else "previously failed tests",
          if (kindsTests.isEmpty) "" else s"${kinds.size} named test categories",
          if (greppedTests.isEmpty) "" else s"${greppedTests.size} tests matching '$grepExpr'",
          if (individualTests.isEmpty) "" else "specified tests"
        ) filterNot (_ == "") mkString ", "
      }

      val allTests: Array[Path] = distinctBy(miscTests ++ kindsTests)(_.toCanonical).sortBy(_.toString).toArray
      val grouped = (allTests groupBy kindOf).toArray sortBy (x => standardKinds indexOf x._1)

      onlyIndividualTests = individualTests.nonEmpty && rerunTests.isEmpty && kindsTests.isEmpty && greppedTests.isEmpty
      totalTests = allTests.size
      expectedFailures = propOrNone("partest.errors") match {
        case Some(num)  => num.toInt
        case _          => 0
      }
      val expectedFailureMessage = if (expectedFailures == 0) "" else s" (expecting $expectedFailures to fail)"
      echo(s"Selected $totalTests tests drawn from $testContributors$expectedFailureMessage\n")
      if (config.optNoExec) echoMixed("Under --no-exec, tests will be compiled but not run! Runnable tests will be marked skipped!")

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
            if (terse) failed.foreach(_.transcript.foreach(echo))
            comment(passFailString(passed.size, failed.size, 0) + " in " + kind)
          }
          echo("")
        }
      }
      this.elapsedMillis = millis
      issueSummaryReport()
    }
    isSuccess
  }

  def banner = {
    val baseDir = fileManager.compilerUnderTest.parent.toString
    def relativize(path: String) = path.replace(baseDir, s"$$baseDir").replace(pathSettings.srcDir.toString, "$sourceDir")
    val vmBin  = javaHome + fileSeparator + "bin"
    val vmName = "%s (build %s, %s)".format(javaVmName, javaVmVersion, javaVmInfo)

    s"""|Partest version:     ${Properties.versionNumberString}
        |Compiler under test: ${relativize(fileManager.compilerUnderTest.getAbsolutePath)}
        |Scala version is:    $versionMsg
        |Scalac options are:  ${(scalacExtraArgs ++ scalacOpts.split(' ')).mkString(" ")}
        |Compilation Path:    ${relativize(FileManager.joinPaths(fileManager.testClassPath))}
        |Java binaries in:    $vmBin
        |Java runtime is:     $vmName
        |Java options are:    $javaOpts
        |baseDir:             $baseDir
        |sourceDir:           ${pathSettings.srcDir}
    """.stripMargin
    // |Available processors:       ${Runtime.getRuntime().availableProcessors()}
    // |Java Classpath:             ${sys.props("java.class.path")}
  }

  def onFinishTest(testFile: File, result: TestState, durationMs: Long): TestState = {
    result
  }

  def runTest(testFile: File): TestState = {
    val start = System.nanoTime()
    val info = TestInfo(testFile)
    val runner = new Runner(info, this)
    var stopwatchDuration: Option[Long] = None

    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    val state =
    if (config.optFailed && !info.logFile.canRead)
      runner.genPass()
    else {
      val (state, durationMs) =
        try runner.run()
        catch {
          case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
        }
      stopwatchDuration = Some(durationMs)
      val verboseSummation = onlyIndividualTests && !terse
      val more = reportTest(state, info, durationMs, diffOnFail = config.optShowDiff || verboseSummation , logOnFail = config.optShowLog || verboseSummation)
      runner.cleanup(state)
      if (more.isEmpty) state
      else {
        state match {
          case f: TestState.Fail => f.copy(transcript = more.toArray)
          case _ => state
        }
      }
    }
    val end = System.nanoTime()
    val durationMs = stopwatchDuration.getOrElse(TimeUnit.NANOSECONDS.toMillis(end - start))
    onFinishTest(testFile, state, durationMs)
  }

  def runTestsForFiles(kindFiles: Array[File], kind: String): Array[TestState] = {
    resetTestNumber(kindFiles.size)

    val pool              = Executors newFixedThreadPool PartestDefaults.numThreads
    val futures           = kindFiles map (f => pool submit callable(runTest(f.getAbsoluteFile)))

    pool.shutdown()
    def aborted = {
      pool.shutdownNow()     // little point in continuing
      // try to get as many completions as possible, in case someone cares
      val results = for (f <- futures) yield {
        try {
          Some(f.get(0, NANOSECONDS))
        } catch {
          case _: Throwable => None
        }
      }
      results.flatten
    }
    Try (pool.awaitTermination(PartestDefaults.waitTime) {
      throw TimeoutException(PartestDefaults.waitTime)
    }) match {
      case Success(_) => futures.map(_.get)
      case Failure(TimeoutException(e)) =>
        warning("Thread pool timeout elapsed before all tests were complete!")
        aborted
      case Failure(ie: InterruptedException) =>
        warning("Thread pool was interrupted")
        ie.printStackTrace()
        aborted
      case Failure(e) =>
        warning("Unexpected failure")
        e.printStackTrace()
        aborted
    }
  }
}
