/* scaladoc, a documentation generator for Scala
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 * @author  Geoffrey Washburn
 */
// $Id$

package scala.tools.nsc

import java.io.File

import scala.tools.nsc.doc.DefaultDocDriver
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos //{Position}


/** The main class for scaladoc, a front-end for the Scala compiler
 *  that generates documentation from source files.
 */
object ScalaDoc {

  val versionMsg = "Scala documentation generator " +
    Properties.versionString + " -- " +
    Properties.copyrightString

  var reporter: ConsoleReporter = _

  def error(msg: String) {
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

  def process(args: Array[String]) {
    val docSettings : doc.Settings = new doc.Settings(error)
    reporter = new ConsoleReporter(docSettings)
    val command = new CompilerCommand(args.toList, docSettings, error, false)
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else {
      if (command.settings.target.value == "msil") {
        val libpath = System.getProperty("msil.libpath")
        if (libpath != null)
          command.settings.assemrefs.value =
            command.settings.assemrefs.value + File.pathSeparator + libpath
      }
      try {
        object compiler extends Global(command.settings, reporter) {
          override protected def computeInternalPhases() {
            phasesSet += syntaxAnalyzer
            phasesSet += analyzer.namerFactory
            phasesSet += analyzer.typerFactory
          }
          override def onlyPresentation = true
        }
        if (reporter.hasErrors) {
          reporter.flush()
          return
        }

        if (command.settings.help.value || command.settings.Xhelp.value || command.settings.Yhelp.value) {
          if (command.settings.help.value) {
              reporter.info(null, command.usageMsg, true)
            reporter.info(null, compiler.pluginOptionsHelp, true)
          }
          if (command.settings.Xhelp.value)
            reporter.info(null, command.xusageMsg, true)
          if (command.settings.Yhelp.value)
            reporter.info(null, command.yusageMsg, true)
        } else if (command.settings.showPlugins.value)
          reporter.info(null, compiler.pluginDescriptions, true)
        else if (command.settings.showPhases.value)
          reporter.info(null, compiler.phaseDescriptions, true)
        else {
            val run = new compiler.Run()
            run compile command.files
            val generator = new DefaultDocDriver {
              lazy val global: compiler.type = compiler
              lazy val settings = docSettings
            }
            generator.process(run.units)
            reporter.printSummary()
        }
      } catch {
        case ex @ FatalError(msg) =>
          if (command.settings.debug.value)
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
    }
  }

  def main(args: Array[String]) {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }
}
