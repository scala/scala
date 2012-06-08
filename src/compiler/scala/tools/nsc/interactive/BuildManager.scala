/* NSC -- new Scala compiler
 * Copyright 2009-2011 Scxala Solutions and LAMP/EPFL
 * @author Iulian Dragos
 * @author Hubert Plocinicak
 */
package scala.tools.nsc
package interactive

import scala.collection._

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.reflect.internal.util.FakePos

import dependencies._
import io.AbstractFile
import language.implicitConversions

trait BuildManager {

  /** Add the given source files to the managed build process. */
  def addSourceFiles(files: Set[AbstractFile])

  /** Remove the given files from the managed build process. */
  def removeFiles(files: Set[AbstractFile])

  /** The given files have been modified by the user. Recompile
   *  them and their dependent files.
   */
  def update(added: Set[AbstractFile], removed: Set[AbstractFile])

  /** Notification that the supplied set of files is being built */
  def buildingFiles(included: Set[AbstractFile]) {}

  /** Load saved dependency information. */
  def loadFrom(file: AbstractFile, toFile: String => AbstractFile) : Boolean

  /** Save dependency information to `file`. */
  def saveTo(file: AbstractFile, fromFile: AbstractFile => String)

  def compiler: scala.tools.nsc.Global

  /** Delete classfiles derived from the supplied set of sources */
  def deleteClassfiles(sources : Set[AbstractFile]) {
    val targets = compiler.dependencyAnalysis.dependencies.targets
    for(source <- sources; cf <- targets(source))
      cf.delete
  }
}


/** Simple driver for testing the build manager. It presents
 *  the user to a 'resident compiler' prompt. Each line is
 *  interpreted as a set of files that have changed. The builder
 *  then derives the dependent files and recompiles them.
 */
object BuildManagerTest extends EvalLoop {

  def prompt = "builder > "

  private def buildError(msg: String) {
    println(msg + "\n  scalac -help  gives more information")
  }

  def main(args: Array[String]) {
    implicit def filesToSet(fs: List[String]): Set[AbstractFile] = {
      def partition(s: String, r: Tuple2[List[AbstractFile], List[String]])= {
	    val v = AbstractFile.getFile(s)
        if (v == null) (r._1, s::r._2) else (v::r._1, r._2)
      }
      val result =  fs.foldRight((List[AbstractFile](), List[String]()))(partition)
      if (!result._2.isEmpty)
        Console.err.println("No such file(s): " + result._2.mkString(","))
      Set.empty ++ result._1
    }

    val settings = new Settings(buildError)
    settings.Ybuildmanagerdebug.value = true
    val command = new CompilerCommand(args.toList, settings)
//    settings.make.value = "off"
//    val buildManager: BuildManager = new SimpleBuildManager(settings)
    val buildManager: BuildManager = new RefinedBuildManager(settings)

    buildManager.addSourceFiles(command.files)

    // enter resident mode
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, settings)
      buildManager.update(command.files, Set.empty)
    }

  }
}
