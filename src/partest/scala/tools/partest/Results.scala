/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools
package partest

import scala.collection.immutable

trait Results {
  self: Universe =>

  /** A collection of tests for a Worker.
   */
  case class TestsToRun(entities: List[TestEntity])

  /** The response from a Worker who has been given TestsToRun.
   */
  case class ResultsOfRun(results: immutable.Map[TestEntity, TestResult])

  /** The result of a single test.  (0: OK, 1: FAILED, 2: TIMEOUT)
   */
  sealed abstract class TestResult(val state: Int, val description: String) {
    def entity: TestEntity

    def passed = state == 0
    def colorize(s: String): String
    def show(msg: String) =
      if (!isShuttingDown)
        showResult(colorize(description), msg)

    private def outputPrefix  = if (isInsideAnt) "" else markNormal("partest: ")
    private def name          = src relativize entity.location   // e.g. "neg/test.scala"
    private def showResult(status: String, extraMsg: String) =
      normal(outputPrefix + "[...]/%-40s [%s] %s\n".format(name, status, extraMsg))

    override def equals(other: Any) = other match {
      case x: TestResult  => entity == x.entity
      case _              => false
    }
    override def hashCode = entity.hashCode
    override def toString = "%s [%s]".format(entity, description)
  }

  class Success(val entity: TestEntity) extends TestResult(0, "   OK   ") {
    def colorize(s: String) = markSuccess(s)
    override def show(msg: String) = if (!isTerse) super.show(msg)
  }
  class Failure(val entity: TestEntity) extends TestResult(1, " FAILED ") {
    def colorize(s: String) = markFailure(s)

    override def show(msg: String) = {
      super.show(msg)

      if (isShowDiff || isTrace)
        normal(entity.diffOutput)

      if (isShowLog || isTrace)
        normal(toStringTrunc(entity.failureMessage(), 1600))
    }
    override def toString = List(super.toString, toStringTrunc(entity.failureMessage(), 400)) mkString "\n"
  }
  class Timeout(val entity: TestEntity) extends TestResult(2, "TIME OUT") {
    def colorize(s: String) = markFailure(s)
  }

  object TestResult {
    def apply(entity: TestEntity, success: Boolean) =
      if (success) new Success(entity)
      else new Failure(entity)

    def apply(entity: TestEntity, state: Int) = state match {
      case 0  => new Success(entity)
      case 1  => new Failure(entity)
      case 2  => new Timeout(entity)
    }
    def unapply(x: Any) = x match {
      case x: TestResult  => Some((x.entity, x.state))
      case _              => None
    }
  }

  /** The combined results of any number of tests.
   */
  case class CombinedTestResults(
    passed: Int,
    failed: Int,
    elapsedMilliseconds: Long,
    failures: List[TestResult]
  ) {
    // housekeeping
    val elapsedSecs = elapsedMilliseconds / 1000
    val elapsedMins = elapsedSecs / 60
    val elapsedHrs  = elapsedMins / 60
    val dispMins = elapsedMins - elapsedHrs  * 60
    val dispSecs = elapsedSecs - elapsedMins * 60

    def total       = passed + failed
    def hasFailures = failed > 0
    def exitCode    = if (expectedErrors == failed) 0 else 1

    def ++(x: CombinedTestResults) = CombinedTestResults(
      passed + x.passed,
      failed + x.failed,
      elapsedMilliseconds + x.elapsedMilliseconds,
      failures ::: x.failures
    )

    def elapsedString   = "%02d:%02d:%02d".format(elapsedHrs, dispMins, dispSecs)
    def failuresString  = {
      if (failures.isEmpty) ""
      else "Summary of failures:" :: failures mkString ("\n", "\n", "")
    }

    override def toString =
      if (total == 0) "There were no tests to run."
      else if (isDryRun) "%d tests would be run." format total
      else if (hasFailures) "%d of %d tests failed (elapsed time: %s)".format(failed, total, elapsedString) + failuresString
      else "All %d tests were successful (elapsed time: %s)".format(total, elapsedString)
  }
}