/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.File

import scala.concurrent.SyncVar

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.{ BatchSourceFile, FakePos } //{Position}

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
      new compiler.Run() compile command.files
    }
  }

  def process(args: Array[String]) {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(args.toList, settings, error, false)
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.Yidedebug.value) {
      command.settings.Xprintpos.value = true
      val compiler = new interactive.Global(command.settings, reporter)
      import compiler._

      val sfs = command.files.map(getSourceFile(_))
      val reloaded = new SyncVar[Either[Unit, Throwable]]
      askReload(sfs, reloaded)
      reloaded.get.right.toOption match {
        case Some(thr) => logError("Failure in presentation compiler", thr)
        case _ =>
      }
      for (sf <- sfs) {
        val cu = unitOf(sf)
        val tree = cu.body
        treePrinters.create(System.out).print(tree)
      }
    }
    else {
      if (command.settings.target.value == "msil") {
        val libpath = System.getProperty("msil.libpath")
        if (libpath != null)
          command.settings.assemrefs.value =
            command.settings.assemrefs.value + File.pathSeparator + libpath
      }
      try {
        val compiler = if (command.settings.Yrangepos.value) new interactive.Global(command.settings, reporter)
        else new Global(command.settings, reporter)

        if (reporter.hasErrors) {
          reporter.flush()
          return
        }

        if (command.shouldStopWithInfo) {
          reporter.info(null, command.getInfoMessage(compiler), true)
        } else {
          if (command.settings.resident.value)
            resident(compiler)
          else if (command.files.isEmpty) {
            reporter.info(null, command.usageMsg, true)
            reporter.info(null, compiler.pluginOptionsHelp, true)
          } else {
            val run = new compiler.Run()
            run compile command.files
            reporter.printSummary()
          }
        }
      } catch {
        case ex @ FatalError(msg) =>
          if (true || command.settings.debug.value) // !!!
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
