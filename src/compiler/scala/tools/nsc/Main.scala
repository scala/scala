/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.File

import scala.tools.nsc.doc.{DocDriver => DocGenerator}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos //{Position}


/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends AnyRef with EvalLoop {

  val versionMsg = "Scala compiler " +
    Properties.versionString + " -- " +
    Properties.copyrightString

  val prompt = Properties.residentPromptString

  var reporter: ConsoleReporter = _

  def error(msg: String) {
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

  /* needed ?? */
  //def errors() = reporter.errors

  def resident(compiler: Global) {
    loop { line =>
      val args = List.fromString(line, ' ')
      val command = new CompilerCommand(args, new Settings(error), error, true)
      (new compiler.Run) compile command.files
    }
  }

  def process(args: Array[String]) {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(List.fromArray(args), settings, error, false)
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
        object compiler extends Global(command.settings, reporter)
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
          if (command.settings.resident.value)
            resident(compiler)
          else if (command.files.isEmpty) {
              reporter.info(null, command.usageMsg, true)
            reporter.info(null, compiler.pluginOptionsHelp, true)
          } else {
            val run = new compiler.Run
            run compile command.files
            if (command.settings.doc.value) {
              object generator extends DocGenerator {
                val global: compiler.type = compiler
                def settings = command.settings
              }
              generator.process(run.units)
            }
            reporter.printSummary()
          }
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
