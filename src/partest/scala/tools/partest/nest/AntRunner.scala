/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.partest
package nest

import java.net.URLClassLoader

// not using any Scala types to ease calling across different scala versions
abstract class AntRunner(srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File, scalacArgs: Array[String]) extends SuiteRunner(
  testSourcePath = Option(srcDir) getOrElse PartestDefaults.sourcePath,
  new FileManager(testClassLoader = testClassLoader),
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

  // called reflectively from scala-partest-test-interface
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
