package scala.tools.nsc

import scala.collection._

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import util.FakePos

import dependencies._
import nsc.io.AbstractFile

trait BuildManager {

  /** Add the given source files to the managed build process. */
  def addSourceFiles(files: Set[AbstractFile])

  /** Remove the given files from the managed build process. */
  def removeFiles(files: Set[AbstractFile])

  /** The given files have been modified by the user. Recompile
   *  them and their dependent files.
   */
  def update(files: Set[AbstractFile])

  /** Load saved dependency information. */
  def loadFrom(file: AbstractFile)

  /** Save dependency information to `file'. */
  def saveTo(file: AbstractFile)

  def compiler: Global
}



/** Simple driver for testing the build manager. It presents
 *  the user to a 'resident compiler' prompt. Each line is
 *  interpreted as a set of files that have changed. The builder
 *  then derives the dependent files and recompiles them.
 */
object BuildManagerTest extends EvalLoop {

  def prompt = "builder > "

  def error(msg: String) {
    println(msg + "\n  scalac -help  gives more information")
  }

  def main(args: Array[String]) {
    implicit def filesToSet(fs: List[String]): Set[AbstractFile] =
      Set.empty ++ (fs map AbstractFile.getFile)

    val settings = new Settings(error)
    val command = new CompilerCommand(List.fromArray(args), settings, error, false)
//    settings.make.value = "off"
    val buildManager: BuildManager = new SimpleBuildManager(settings)

    buildManager.addSourceFiles(command.files)

    // enter resident mode
    loop { line =>
      val args = List.fromString(line, ' ')
      val command = new CompilerCommand(args, new Settings(error), error, true)
      buildManager.update(command.files)
    }

  }
}
