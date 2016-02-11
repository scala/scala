/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.partest
package nest

import _root_.sbt.testing._
import java.net.URLClassLoader
import TestState._

// called reflectively from scala-partest-test-interface
class SBTRunner(partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger],
    srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File, scalacArgs: Array[String], args: Array[String])
    extends AbstractRunner(args.filter(a => !a.startsWith("-D")).mkString(" ")) {

  // no summary, SBT will do that for us
  summarizing = true

  val javaOpts = {
    val l = args.filter(_.startsWith("-Dpartest.java_opts=")).map(_.substring(20))
    if(l.isEmpty) PartestDefaults.javaOpts
    else l.mkString(" ")
  }

  override val suiteRunner = new SuiteRunner(
    testSourcePath = optSourcePath orElse Option(srcDir) getOrElse PartestDefaults.sourcePath,
    new FileManager(testClassLoader = testClassLoader),
    updateCheck = optUpdateCheck,
    failed  = optFailed,
    javaCmdPath = Option(javaCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javaCmd,
    javacCmdPath = Option(javacCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javacCmd,
    scalacExtraArgs = scalacArgs,
    javaOpts = javaOpts) {

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
    case Uninitialized(_) => (Status.Pending, new OptionalThrowable)
    case Pass(_)          => (Status.Success, new OptionalThrowable)
    case Updated(_)       => (Status.Success, new OptionalThrowable)
    case Skip(_, _)       => (Status.Skipped, new OptionalThrowable)
    case Fail(_, _, _)    => (Status.Failure, new OptionalThrowable)
    case Crash(_, e, _)   => (Status.Error, new OptionalThrowable(e))
  }
}
