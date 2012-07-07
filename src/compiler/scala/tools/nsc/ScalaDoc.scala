/* scaladoc, a documentation generator for Scala
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 * @author  Geoffrey Washburn
 */

package scala.tools.nsc

import java.io.File.pathSeparator
import scala.tools.nsc.doc.DocFactory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.internal.util.FakePos
import Properties.msilLibPath

/** The main class for scaladoc, a front-end for the Scala compiler
 *  that generates documentation from source files.
 */
class ScalaDoc {
  val versionMsg = "Scaladoc %s -- %s".format(Properties.versionString, Properties.copyrightString)

  def process(args: Array[String]): Boolean = {
    var reporter: ConsoleReporter = null
    val docSettings = new doc.Settings(msg => reporter.error(FakePos("scaladoc"), msg + "\n  scaladoc -help  gives more information"),
                                       msg => reporter.printMessage(msg))
    reporter = new ConsoleReporter(docSettings) {
      // need to do this so that the Global instance doesn't trash all the
      // symbols just because there was an error
      override def hasErrors = false
    }
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
    else try {
      if (docSettings.target.value == "msil")
        msilLibPath foreach (x => docSettings.assemrefs.value += (pathSeparator + x))

      new DocFactory(reporter, docSettings) document command.files
    }
    catch {
      case ex @ FatalError(msg) =>
        if (docSettings.debug.value) ex.printStackTrace()
        reporter.error(null, "fatal error: " + msg)
    }
    finally reporter.printSummary()

    // not much point in returning !reporter.hasErrors when it has
    // been overridden with constant false.
    true
  }
}

object ScalaDoc extends ScalaDoc {
  class Command(arguments: List[String], settings: doc.Settings) extends CompilerCommand(arguments, settings) {
    override def cmdName = "scaladoc"
    override def usageMsg = (
      createUsageMsg("where possible scaladoc", false, x => x.isStandard && settings.isScaladocSpecific(x.name)) +
      "\n\nStandard scalac options also available:" +
      createUsageMsg(x => x.isStandard && !settings.isScaladocSpecific(x.name))
    )
  }

  def main(args: Array[String]): Unit = sys exit {
    if (process(args)) 0 else 1
  }
}
