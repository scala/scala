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

package scala.tools.nsc

import scala.tools.nsc.doc.DocFactory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.settings.DefaultPathFactory
import scala.reflect.internal.util.{FakePos, Position}

/** The main class for scaladoc, a front-end for the Scala compiler
 *  that generates documentation from source files.
 */
class ScalaDoc {
  import ScalaDoc._
  val versionMsg = s"Scaladoc ${Properties.versionString} -- ${Properties.copyrightString}"

  def process(args: Array[String]): Boolean = {
    var reporter: ScalaDocReporter = null
    val docSettings = new doc.Settings(msg => reporter.error(NoDocPos, s"$msg\n  scaladoc -help gives more information"),
                                       msg => reporter.echo(msg),
                                       DefaultPathFactory)
    reporter = new ScalaDocReporter(docSettings)
    val command = new Command(args.toList, docSettings)
    def hasFiles = command.files.nonEmpty || docSettings.uncompilableFiles.nonEmpty

    if (!command.ok)
      ()
    else if (docSettings.version.value)
      reporter.echo(versionMsg)
    else if (docSettings.Xhelp.value)
      reporter.echo(command.xusageMsg)
    else if (docSettings.Yhelp.value)
      reporter.echo(command.yusageMsg)
    else if (docSettings.showPlugins.value)
      reporter.warning(NoDocPos, "Plugins are not available when using Scaladoc")
    else if (docSettings.showPhases.value)
      reporter.warning(NoDocPos, s"Phases are restricted when using Scaladoc.\n${new DocFactory(reporter, docSettings).compiler.phaseDescriptions}")
    else if (docSettings.help.value || !hasFiles)
      reporter.echo(command.usageMsg)
    else
      try new DocFactory(reporter, docSettings).document(command.files)
      catch {
        case ex @ FatalError(msg) =>
          if (docSettings.isDebug) ex.printStackTrace()
          reporter.error(NoDocPos, s"fatal error: $msg")
      }
      finally reporter.finish()

    !reporter.reallyHasErrors
  }
}

/** The Scaladoc reporter adds summary messages to the `ConsoleReporter`
 *
 *  Use the `summaryX` methods to add unique summarizing message to the end of
 *  the run.
 */
class ScalaDocReporter(settings: Settings) extends ConsoleReporter(settings) {
  import scala.collection.mutable.LinkedHashMap

  // need to do sometimes lie so that the Global instance doesn't
  // trash all the symbols just because there was an error
  override def hasErrors = false
  def reallyHasErrors = super.hasErrors

  private[this] val delayedMessages: LinkedHashMap[(Position, String), () => Unit] =
    LinkedHashMap.empty

  /** Eliminates messages if both `pos` and `msg` are equal to existing element */
  def addDelayedMessage(pos: Position, msg: String, print: () => Unit): Unit =
    delayedMessages += ((pos, msg) -> print)

  def printDelayedMessages(): Unit = delayedMessages.values.foreach(_.apply())

  override def finish(): Unit = {
    printDelayedMessages()
    super.finish()
  }
}

object ScalaDoc extends ScalaDoc {
  val NoDocPos = FakePos("scaladoc")

  class Command(arguments: List[String], settings: doc.Settings) extends CompilerCommand(arguments, settings) {
    override def cmdName = "scaladoc"
    override def usageMsg =
      sm"""${createUsageMsg("where possible scaladoc", explain = false)(x => x.isStandard && settings.isScaladocSpecific(x.name))}
           |Standard scalac options also available:
           |${optionsMessage(x => x.isStandard && !settings.isScaladocSpecific(x.name))}"""
  }

  def main(args: Array[String]): Unit = {
    System.exit(if (process(args)) 0 else 1)
  }
}
