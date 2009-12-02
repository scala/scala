/* scaladoc, a documentation generator for Scala
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 * @author  Geoffrey Washburn
 */
// $Id$

package scala.tools.nsc

import java.io.File

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos //{Position}


/** The main class for scaladoc, a front-end for the Scala compiler
 *  that generates documentation from source files.
 */
object ScalaDoc {

  val versionMsg: String =
    "Scaladoc " +
    Properties.versionString + " -- " +
    Properties.copyrightString

  var reporter: ConsoleReporter = _

  def error(msg: String): Unit = {
    reporter.error(FakePos("scalac"), msg + "\n  scalac -help  gives more information")
  }

  def process(args: Array[String]): Unit = {

    val docSettings: doc.Settings =
      new doc.Settings(error)

    reporter = new ConsoleReporter(docSettings)

    val command =
      new CompilerCommand(args.toList, docSettings, error, false)

    if (!reporter.hasErrors) { // No need to continue if reading the command generated errors

      if (docSettings.version.value)
        reporter.info(null, versionMsg, true)
      else if (docSettings.help.value) {
        reporter.info(null, command.usageMsg, true)
      }
      else if (docSettings.Xhelp.value)
        reporter.info(null, command.xusageMsg, true)
      else if (docSettings.Yhelp.value)
        reporter.info(null, command.yusageMsg, true)
      else if (docSettings.showPlugins.value)
        reporter.warning(null, "Plugins are not available when using Scaladoc")
      else if (docSettings.showPhases.value)
        reporter.warning(null, "Phases are restricted when using Scaladoc")
      else try {

        if (docSettings.target.value == "msil") {
          val libpath = System.getProperty("msil.libpath")
          if (libpath != null)
            docSettings.assemrefs.value = docSettings.assemrefs.value + File.pathSeparator + libpath
        }

        val docProcessor = new scala.tools.nsc.doc.DocFactory(reporter, docSettings)
        docProcessor.document(command.files)

      }
      catch {
        case ex @ FatalError(msg) =>
          if (docSettings.debug.value) ex.printStackTrace();
          reporter.error(null, "fatal error: " + msg)
      }
      finally {
        reporter.printSummary()
      }
    }

  }

  def main(args: Array[String]): Unit = {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }

}
