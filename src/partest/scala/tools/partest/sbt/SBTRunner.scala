/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest.sbt

import java.net.URLClassLoader

import _root_.sbt.testing._

import scala.tools.partest.TestState._
import scala.tools.partest._
import scala.tools.partest.nest.{AbstractRunner, FileManager, RunnerSpec}

class SBTRunner(config: RunnerSpec.Config,
                partestFingerprint: Fingerprint, eventHandler: EventHandler, loggers: Array[Logger],
                srcDir: String, testClassLoader: URLClassLoader, javaCmd: File, javacCmd: File,
                scalacArgs: Array[String], args: Array[String]) extends AbstractRunner(
  config,
  config.optSourcePath orElse Option(srcDir) getOrElse PartestDefaults.sourcePath,
  new FileManager(testClassLoader = testClassLoader)
) {

  // no summary, SBT will do that for us
  override protected val printSummary = false
  override protected val partestCmd   = "partest"

  val defs = {
    val Def = "-D([^=]*)=(.*)".r
    args.collect { case Def(k, v) => (k, v) }
  }

  // Enable colors if there's an explicit override or all loggers support them
  override val log = new ConsoleLog({
    val ptOverride = defs.collect { case ("partest.colors", v) => v.toBoolean }.lastOption
    ptOverride.getOrElse {
      val sbtOverride1 = sys.props.get("sbt.log.format").map(_.toBoolean)
      val sbtOverride2 = sys.props.get("sbt.log.noformat").map(s => !s.toBoolean)
      sbtOverride1.orElse(sbtOverride2).getOrElse {
        loggers.forall(_.ansiCodesSupported())
      }
    }
  })

  override val javaOpts = {
    val l = defs.collect { case ("partest.java_opts", v) => v }
    if(l.isEmpty) PartestDefaults.javaOpts
    else l.mkString(" ")
  }

  override val scalacOpts = {
    val l = defs.collect { case ("partest.scalac_opts", v) => v }
    if(l.isEmpty) PartestDefaults.scalacOpts
    else l.mkString(" ")
  }

  override val javaCmdPath = Option(javaCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javaCmd
  override val javacCmdPath = Option(javacCmd).map(_.getAbsolutePath) getOrElse PartestDefaults.javacCmd
  override val scalacExtraArgs = scalacArgs.toIndexedSeq

  override def onFinishTest(testFile: File, result: TestState, durationMs: Long): TestState = {
    synchronized {
      eventHandler.handle(new Event {
        def fullyQualifiedName: String = pathSettings.testRoot.name + "/" + testSourcePath
        def fingerprint: Fingerprint = partestFingerprint
        def selector: Selector = new TestSelector(testFile.testIdent)
        val (status, throwable) = makeStatus(result)
        def duration: Long = durationMs
      })
    }
    result
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
