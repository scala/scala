/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.File
import File.pathSeparator

import scala.concurrent.SyncVar

import scala.tools.nsc.interactive.{ RefinedBuildManager, SimpleBuildManager }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.{ BatchSourceFile, FakePos } //{Position}
import Properties.{ versionString, copyrightString, residentPromptString, msilLibPath }

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends AnyRef with EvalLoop {

  val versionMsg = "Scala compiler " +
    versionString + " -- " +
    copyrightString

  val prompt = residentPromptString

  var reporter: ConsoleReporter = _

  def error(msg: String) {
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

  /* needed ?? */
  //def errors() = reporter.errors

  def resident(compiler: Global) {
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, new Settings(error))
      compiler.reporter.reset
      new compiler.Run() compile command.files
    }
  }

  def process(args: Array[String]) {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(args.toList, settings)
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else if (command.settings.Yidedebug.value) {
      command.settings.Xprintpos.value = true
      command.settings.Yrangepos.value = true
      val compiler = new interactive.Global(command.settings, reporter)
      import compiler.{ reporter => _, _ }

      val sfs = command.files.map(getSourceFile(_))
      val reloaded = new SyncVar[Either[Unit, Throwable]]
      askReload(sfs, reloaded)
      reloaded.get.right.toOption match {
        case Some(ex) => reporter.cancelled = true // Causes exit code to be non-0
        case None => reporter.reset // Causes other compiler errors to be ignored
      }
      askShutdown
    } else if (command.settings.Ybuilderdebug.value != "none") {
      def fileSet(files : List[String]) = Set.empty ++ (files map AbstractFile.getFile)

      val buildManager = if (command.settings.Ybuilderdebug.value == "simple")
        new SimpleBuildManager(settings)
      else
        new RefinedBuildManager(settings)

      buildManager.addSourceFiles(fileSet(command.files))

      // enter resident mode
      loop { line =>
        val args = line.split(' ').toList
        val command = new CompilerCommand(args.toList, settings)
        buildManager.update(fileSet(command.files), Set.empty)
      }
    } else {
      if (command.settings.target.value == "msil")
        msilLibPath foreach (x => command.settings.assemrefs.value += (pathSeparator + x))

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
