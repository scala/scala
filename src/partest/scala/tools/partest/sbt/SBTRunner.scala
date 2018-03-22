/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.partest.sbt

import java.net.URLClassLoader

import _root_.sbt.testing._

import scala.tools.partest.TestState._
import scala.tools.partest._
import scala.tools.partest.nest.{AbstractRunner, FileManager, RunnerSpec, SuiteRunner}

class SBTRunner(val config: RunnerSpec.Config,
                partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger],
                srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File,
                scalacArgs: Array[String], args: Array[String]) extends AbstractRunner {

  // no summary, SBT will do that for us
  override protected val printSummary = false
  override protected val partestCmd   = "partest"

  val defs = {
    val Def = "-D([^=]*)=(.*)".r
    args.collect { case Def(k, v) => (k, v) }
  }

  // Enable colors if there's an explicit override or all loggers support them
  override protected val colorEnabled = {
    val ptOverride = defs.collect { case ("partest.colors", v) => v.toBoolean }.lastOption
    ptOverride.getOrElse {
      val sbtOverride1 = sys.props.get("sbt.log.format").map(_.toBoolean)
      val sbtOverride2 = sys.props.get("sbt.log.noformat").map(s => !s.toBoolean)
      sbtOverride1.orElse(sbtOverride2).getOrElse {
        loggers.forall(_.ansiCodesSupported())
      }
    }
  }

  val javaOpts = {
    val l = defs.collect { case ("partest.java_opts", v) => v }
    if(l.isEmpty) PartestDefaults.javaOpts
    else l.mkString(" ")
  }

  val scalacOpts = {
    val l = defs.collect { case ("partest.scalac_opts", v) => v }
    if(l.isEmpty) PartestDefaults.javaOpts
    else l.mkString(" ")
  }

  private val testSrcPath: String = config.optSourcePath orElse Option(srcDir) getOrElse PartestDefaults.sourcePath

  val suiteRunner = new SuiteRunner(
    testSourcePath = testSrcPath,
    new FileManager(testClassLoader = testClassLoader),
    updateCheck = config.optUpdateCheck,
    failed  = config.optFailed,
    nestUI = nestUI,
    javaCmdPath = Option(javaCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javaCmd,
    javacCmdPath = Option(javacCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javacCmd,
    scalacExtraArgs = scalacArgs.toIndexedSeq,
    javaOpts = javaOpts,
    scalacOpts = scalacOpts) { self =>

      override def onFinishTest(testFile: File, result: TestState, durationMs: Long): TestState = {
        self.synchronized {
          eventHandler.handle(new Event {
            def fullyQualifiedName: String = scala.tools.partest.nest.PathSettings.testRoot.name + "/" + testSrcPath
            def fingerprint: Fingerprint = partestFingerprint
            def selector: Selector = new TestSelector(testFile.testIdent)
            val (status, throwable) = makeStatus(result)
            def duration: Long = durationMs
          })
        }
        result
      }
    }

  def makeStatus(t: TestState): (Status, OptionalThrowable) = t match {
    case Uninitialized(_) => (Status.Pending, new OptionalThrowable)
    case Pass(_)          => (Status.Success, new OptionalThrowable)
    case Updated(_)       => (Status.Success, new OptionalThrowable)
    case Skip(_, _)       => (Status.Skipped, new OptionalThrowable)
    case Fail(_, reason, transcript)    => (Status.Failure, new OptionalThrowable(new TestFailedThrowable(reason, transcript.mkString("\n"))))
    case Crash(_, e, _)   => (Status.Error, new OptionalThrowable(e))
  }
}
class TestFailedThrowable(reason: String, transcript: String) extends Throwable(reason + "\n\n" + transcript)
