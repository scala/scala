/* scaladoc, a documentation generator for Scala
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 * @author  Geoffrey Washburn
 */

package scala.tools.nsc

import scala.tools.nsc.doc.DocFactory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.Reporter
import scala.reflect.internal.util.{ FakePos, NoPosition, Position }

/** The main class for scaladoc, a front-end for the Scala compiler
 *  that generates documentation from source files.
 */
class ScalaDoc {
  val versionMsg = "Scaladoc %s -- %s".format(Properties.versionString, Properties.copyrightString)

  def process(args: Array[String]): Boolean = {
    var reporter: ScalaDocReporter = null
    val docSettings = new doc.Settings(msg => reporter.error(FakePos("scaladoc"), msg + "\n  scaladoc -help  gives more information"),
                                       msg => reporter.printMessage(msg))
    reporter = new ScalaDocReporter(docSettings)
    val command = new ScalaDoc.Command(args.toList, docSettings)
    def hasFiles = command.files.nonEmpty || docSettings.uncompilableFiles.nonEmpty

    if (docSettings.version.value)
      reporter.echo(versionMsg)
    else if (docSettings.Xhelp.value)
      reporter.echo(command.xusageMsg)
    else if (docSettings.Yhelp.value)
      reporter.echo(command.yusageMsg)
    else if (docSettings.showPlugins.value)
      reporter.warning(null, "Plugins are not available when using Scaladoc")
    else if (docSettings.showPhases.value)
      reporter.warning(null, "Phases are restricted when using Scaladoc")
    else if (docSettings.help.value || !hasFiles)
      reporter.echo(command.usageMsg)
    else
      try { new DocFactory(reporter, docSettings) document command.files }
      catch {
        case ex @ FatalError(msg) =>
          if (docSettings.debug.value) ex.printStackTrace()
          reporter.error(null, "fatal error: " + msg)
      }
      finally reporter.printSummary()

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

  override def printSummary(): Unit = {
    printDelayedMessages()
    super.printSummary()
  }
}

object ScalaDoc extends ScalaDoc {
  class Command(arguments: List[String], settings: doc.Settings) extends CompilerCommand(arguments, settings) {
    override def cmdName = "scaladoc"
    override def usageMsg = (
      createUsageMsg("where possible scaladoc", shouldExplain = false, x => x.isStandard && settings.isScaladocSpecific(x.name)) +
      "\n\nStandard scalac options also available:" +
      createUsageMsg(x => x.isStandard && !settings.isScaladocSpecific(x.name))
    )
  }

  def main(args: Array[String]): Unit = sys exit {
    if (process(args)) 0 else 1
  }

  implicit class SummaryReporter(val rep: Reporter) extends AnyVal {
    /** Adds print lambda to ScalaDocReporter, executes it on other reporter */
    private[this] def summaryMessage(pos: Position, msg: String, print: () => Unit): Unit = rep match {
      case r: ScalaDocReporter => r.addDelayedMessage(pos, msg, print)
      case _ => print()
    }

    def summaryEcho(pos: Position, msg: String): Unit    = summaryMessage(pos, msg, () => rep.echo(pos, msg))
    def summaryError(pos: Position, msg: String): Unit   = summaryMessage(pos, msg, () => rep.error(pos, msg))
    def summaryWarning(pos: Position, msg: String): Unit = summaryMessage(pos, msg, () => rep.warning(pos, msg))

    def summaryEcho(msg: String): Unit    = summaryEcho(NoPosition, msg)
    def summaryError(msg: String): Unit   = summaryError(NoPosition, msg)
    def summaryWarning(msg: String): Unit = summaryWarning(NoPosition, msg)
  }
}
