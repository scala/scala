/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.File
import File.pathSeparator

import scala.tools.nsc.interactive.{ RefinedBuildManager, SimpleBuildManager }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.reflect.internal.util.{ BatchSourceFile, FakePos } //{Position}
import Properties.{ versionString, copyrightString, residentPromptString, msilLibPath }

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends Driver with EvalLoop {

  val prompt = residentPromptString

  def resident(compiler: Global) {
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, new Settings(scalacError))
      compiler.reporter.reset()
      new compiler.Run() compile command.files
    }
  }

  override def processSettingsHook(): Boolean =
    if (settings.Yidedebug.value) {
      settings.Xprintpos.value = true
      settings.Yrangepos.value = true
      val compiler = new interactive.Global(settings, reporter)
      import compiler.{ reporter => _, _ }

      val sfs = command.files map getSourceFile
      val reloaded = new interactive.Response[Unit]
      askReload(sfs, reloaded)

      reloaded.get.right.toOption match {
        case Some(ex) => reporter.cancelled = true // Causes exit code to be non-0
        case None => reporter.reset() // Causes other compiler errors to be ignored
      }
      askShutdown
      false
    }
    else if (settings.Ybuilderdebug.value != "none") {
      def fileSet(files : List[String]) = Set.empty ++ (files map AbstractFile.getFile)

      val buildManager = settings.Ybuilderdebug.value match {
        case "simple"   => new SimpleBuildManager(settings)
        case _          => new RefinedBuildManager(settings)
      }
      buildManager.addSourceFiles(fileSet(command.files))

      // enter resident mode
      loop { line =>
        val args = line.split(' ').toList
        val command = new CompilerCommand(args.toList, settings)
        buildManager.update(fileSet(command.files), Set.empty)
      }
      false
    }
    else {
      if (settings.target.value == "msil")
        msilLibPath foreach (x => settings.assemrefs.value += (pathSeparator + x))
      true
    }

  override def newCompiler(): Global =
    if (settings.Yrangepos.value) new Global(settings, reporter) with interactive.RangePositions
    else Global(settings, reporter)

  override def doCompile(compiler: Global) {
    if (settings.resident.value)
      resident(compiler)
    else super.doCompile(compiler)
  }
}
