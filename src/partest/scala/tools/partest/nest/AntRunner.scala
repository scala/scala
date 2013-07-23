/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.partest
package nest

import sbt.testing.EventHandler
import sbt.testing.Logger
import sbt.testing.Event
import sbt.testing.Fingerprint
import sbt.testing.Selector
import sbt.testing.Status
import sbt.testing.OptionalThrowable
import sbt.testing.SuiteSelector
import sbt.testing.TestSelector

// not using any Scala types to ease calling across different scala versions
abstract class AntRunner(compilationPaths: Array[String], javaCmd: File, javacCmd: File, scalacArgs: Array[String]) extends SuiteRunner(
  new FileManager(testClassPath = compilationPaths map { fs => Path(fs) } toList),
  updateCheck = false,
  failed  = false,
  javaCmdPath = Option(javaCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javaCmd,
  javacCmdPath = Option(javacCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javacCmd,
  scalacExtraArgs = scalacArgs) {

  def error(msg: String): Nothing = sys.error(msg)
  def echo(msg: String): Unit
  def log(msg: String): Unit
  def onFinishKind(kind: String, passed: Array[TestState], failed: Array[TestState]): Unit

  final def runSet(kind: String, files: Array[File]): (Int, Int, Array[String]) = {
    if (files.isEmpty) (0, 0, Array.empty[String])
    else {
      log(s"Running ${files.length} tests in '$kind' at $now")
      // log(s"Tests: ${files.toList}")
      val results = runTestsForFiles(files, kind)
      val (passed, failed) = results partition (_.isOk)
      val numPassed = passed.size
      val numFailed = failed.size
      def failedMessages = failed map (_.longStatus)

      onFinishKind(kind, passed, failed)

      (numPassed, numFailed, failedMessages)
    }
  }

  final def execute(kinds: Array[String]): String = {
    echo(banner)

    val _results = kinds map (k => runSet(k, TestKinds testsFor k map (_.jfile) toArray))

    val allSuccesses = _results map (_._1) sum
    val allFailures = _results map (_._2) sum
    val allFailedPaths = _results flatMap (_._3)

    if (allFailures > 0)
      s"Test suite finished with $allFailures case${if (allFailures > 1) "s" else ""} failing:\n" +
        allFailedPaths.mkString("\n")
    else if (allSuccesses == 0) "There were no tests to run."
    else "Test suite finished with no failures."
  }
}

class SBTRunner(partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger], compilationPaths: Array[String], javaCmd: File, javacCmd: File, scalacArgs: Array[String]) extends AntRunner(compilationPaths, javaCmd, javacCmd, scalacArgs) {
  override def error(msg: String): Nothing = sys.error(msg)
  def echo(msg: String): Unit = loggers foreach { l => l.info(msg) }
  def log(msg: String): Unit = loggers foreach { l => l.debug(msg) }
  def onFinishKind(kind: String, passed: Array[TestState], failed: Array[TestState]): Unit =
    eventHandler.handle(new Event {
      def fullyQualifiedName: String = kind
      def fingerprint: Fingerprint = partestFingerprint
      def selector: Selector = new SuiteSelector
      def status: Status = if (failed.isEmpty) Status.Success else Status.Failure
      def throwable: OptionalThrowable = new OptionalThrowable
      def duration: Long = -1
    })

  override def onFinishTest(testFile: File, result: TestState): TestState = {
    eventHandler.handle(new Event {
      def fullyQualifiedName: String = testFile.testIdent
      def fingerprint: Fingerprint = partestFingerprint
      def selector: Selector = new TestSelector(testFile.testIdent)
      def status: Status = if (result.isOk) Status.Success else Status.Failure
      def throwable: OptionalThrowable = new OptionalThrowable
      def duration: Long = -1
    })
    result
  }
}
