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
import java.net.URLClassLoader
import TestState._

// called reflectively from scala-partest-test-interface
class SBTRunner(partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger],
    srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File, scalacArgs: Array[String], args: Array[String])
    extends AbstractRunner(args.mkString(" ")) {

  // no summary, SBT will do that for us
  summarizing = true

  override val suiteRunner = new SuiteRunner(
    testSourcePath = Option(srcDir) getOrElse PartestDefaults.sourcePath,
    new FileManager(testClassLoader = testClassLoader),
    updateCheck = optUpdateCheck,
    failed  = optFailed,
    javaCmdPath = Option(javaCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javaCmd,
    javacCmdPath = Option(javacCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javacCmd,
    scalacExtraArgs = scalacArgs) {

      override def onFinishTest(testFile: File, result: TestState): TestState = {
        eventHandler.handle(new Event {
          def fullyQualifiedName: String = testFile.testIdent
          def fingerprint: Fingerprint = partestFingerprint
          def selector: Selector = new TestSelector(testFile.testIdent)
          val (status, throwable) = makeStatus(result)
          def duration: Long = -1
        })
        result
      }
    }

  def makeStatus(t: TestState): (Status, OptionalThrowable) = t match {
    case _: Uninitialized => (Status.Pending, new OptionalThrowable)
    case _: Pass => (Status.Success, new OptionalThrowable)
    case _: Updated => (Status.Success, new OptionalThrowable)
    case _: Skip => (Status.Skipped, new OptionalThrowable)
    case _: Fail => (Status.Failure, new OptionalThrowable)
    case Crash(_,e,_) => (Status.Error, new OptionalThrowable(e))
  }
}
