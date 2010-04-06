/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest

import scala.tools.nsc.io._
import scala.actors.{ Actor, TIMEOUT }
import scala.actors.Actor._
import scala.collection.immutable
import scala.util.control.Exception.ultimately

/** The machinery for concurrent execution of tests.  Each Worker
 *  is given a bundle of tests, which it runs sequentially and then
 *  sends a report back to the dispatcher.
 */
trait Dispatcher {
  partest: Universe =>

  /** The public entry point.  The given filter narrows down the list of
   *  tests to run.
   */
  def runSelection(categories: List[TestCategory], filt: TestEntity => Boolean = _ => true): CombinedTestResults = {
    // Setting scala.home informs tests where to obtain their jars.
    setProp("scala.home", testBuildDir.path)

    val allTests  = allCategories flatMap (_.enumerate)
    val selected  = allTests filter filt
    val groups    = selected groupBy (_.category)
    val count     = selected.size

    if (count == 0) return CombinedTestResults(0, 0, 0)
    else if (count == allTests.size) verbose("Running all %d tests." format count)
    else verbose("Running %d/%d tests: %s".format(count, allTests.size, toStringTrunc(selected map (_.label) mkString ", ")))

    allCategories collect { case x if groups contains x => runCategory(x, groups(x)) } reduceLeft (_ ++ _)
  }

  private def parallelizeTests(tests: List[TestEntity]): immutable.Map[TestEntity, Int] = {
    // propagate verbosity
    if (isDebug) scala.actors.Debug.level = 3

    // "If elected, I guarantee a slice of tests for every worker!"
    val groups = tests grouped ((tests.size / numWorkers) + 1) toList

    // "Workers, line up for assignments!"
    val workers =
      for ((slice, workerNum) <- groups.zipWithIndex) yield {
        returning(new Worker(workerNum)) { worker =>
          worker.start()
          worker ! TestsToRun(slice)
        }
      }

    normal("Started %d workers with ~%d tests each.\n".format(groups.size, groups.head.size))

    /** Listening for news from the proletariat.
     */
    (workers map { w =>
      receiveWithin(workerTimeout * 1000) {
        case ResultsOfRun(resultMap)  => resultMap
        case TIMEOUT                  =>
          warning("Worker %d timed out." format w.workerNum)
          immutable.Map[TestEntity, Int]()
          // mark all the worker's tests as having timed out - should be hard to miss
          groups(w.workerNum) map (_ -> 2) toMap
      }
    }) reduceLeft (_ ++ _)
  }

  private def runCategory(category: TestCategory, tests: List[TestEntity]): CombinedTestResults = {
    val kind = category.kind
    normal("%s (%s tests in %s)\n".format(category.startMessage, tests.size, category))

    val (milliSeconds, resultMap) = timed2(parallelizeTests(tests))
    val (passed, failed)          = resultsToStatistics(resultMap)

    CombinedTestResults(passed, failed, milliSeconds)
  }

  /** A Worker is given a bundle of tests and runs them all sequentially.
   */
  class Worker(val workerNum: Int) extends Actor {
    def act() {
      react { case TestsToRun(tests) =>
        val master = sender
        runTests(tests)(results => master ! ResultsOfRun(results))
      }
    }

    /** Runs the tests.  Passes the result Map to onCompletion when done.
     */
    private def runTests(tests: List[TestEntity])(onCompletion: immutable.Map[TestEntity, Int] => Unit) {
      var results       = new immutable.HashMap[TestEntity, Int] // maps tests to results
      val numberOfTests = tests.size
      val testIterator  = tests.iterator
      def processed     = results.size
      def isComplete    = testIterator.isEmpty

      def atThreshold(num: Double) = {
        require(num >= 0 && num <= 1.0)
        ((processed - 1).toDouble / numberOfTests <= num) && (processed.toDouble / numberOfTests >= num)
      }

      def extraMessage = {
        // for now quiet for normal people
        if (isVerbose || isTrace || isDebug) {
          if (isComplete) "(#%d 100%%)" format workerNum
          else if (isVerbose) "(#%d %d/%d)".format(workerNum, processed, numberOfTests)
          else if (isTrace && atThreshold(0.5)) "(#%d 50%%)" format workerNum
          else ""
        }
        else ""
      }

      def countAndReport(result: TestResult) {
        val TestResult(test, state) = result
        // refuse to count an entity twice
        if (results contains test)
          return warning("Received duplicate result for %s: was %s, now %s".format(test, results(test), state))

        // increment the counter for this result state
        results += (test -> state)

        // show on screen
        if (isDryRun) normal("\n")   // blank line between dry run traces
        else result show extraMessage

        // remove log if successful
        if (result.passed)
          test.deleteLog()

        // Respond to master if this Worker is complete
        if (isComplete)
          onCompletion(results)
      }

      Actor.loopWhile(testIterator.hasNext) {
        val parent = self
        // pick a test and set some alarms
        val test    = testIterator.next
        val alarmer = test startAlarms (parent ! new Timeout(test))

        actor {
          ultimately(alarmer.cancelAll()) {
            // Calling isSuccess forces the lazy val "process" inside the test, running it.
            val res = test.isSuccess
            // Cancel the alarms and alert the media.
            parent ! TestResult(test, res)
          }
        }

        react {
          case x: TestResult  => countAndReport(x)
        }
      }
    }
  }
}