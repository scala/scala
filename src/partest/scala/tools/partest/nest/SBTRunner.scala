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

// called reflectively from scala-partest-test-interface
class SBTRunner(partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger],
    srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File, scalacArgs: Array[String])
    extends AntRunner(srcDir, testClassLoader, javaCmd, javacCmd, scalacArgs) {
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
