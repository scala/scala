/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import scala.tools.nsc.doc.DocGenerator
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.FakePos //{Position}


/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends AnyRef with EvalLoop {

  val versionMsg = "Scala compiler " +
    Properties.versionString + " -- " +
    Properties.copyrightString
  val prompt = "\nnsc> "

  var reporter: ConsoleReporter = _

  def error(msg: String): unit =
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")

  /* needed ?? */
  def errors() = reporter.errors

  def resident(compiler: Global): unit =
    loop { line =>
      val args = List.fromString(line, ' ')
      val command = new CompilerCommand(args, new Settings(error), error, true)
      (new compiler.Run) compile command.files
    }

  def process(args: Array[String]): unit = {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(List.fromArray(args), settings, error, false)
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.help.value)
      reporter.info(null, command.usageMsg, true)
    else {
      try {
        object compiler extends Global(command.settings, reporter);
        if (command.settings.resident.value)
          resident(compiler)
        else if (command.files.isEmpty)
          reporter.info(null, command.usageMsg, true)
        else {
          val run = new compiler.Run
          run compile command.files
          if (command.settings.doc.value) {
            object generator extends DocGenerator {
              val global : compiler.type = compiler
              def outdir = command.settings.outdir.value
              def windowTitle = command.settings.windowtitle.value
              def documentTitle = command.settings.documenttitle.value
            };
            generator.process(run.units)
          }
        }
      } catch {
        case ex @ FatalError(msg) =>
          if (command.settings.debug.value)
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
      reporter.printSummary()
    }
  }

  def main(args: Array[String]): unit = {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }

}
