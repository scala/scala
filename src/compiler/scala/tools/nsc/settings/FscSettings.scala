/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package settings

import util.ClassPath
import io.{ Path, AbstractFile }

class FscSettings(error: String => Unit) extends Settings(error) {
  outer =>

  locally {
    disable(prompt)
    disable(resident)
  }

  val currentDir   = StringSetting ("-current-dir", "path", "Base directory for resolving relative paths", "").internalOnly()
  val reset        = BooleanSetting("-reset",    "Reset compile server caches")
  val shutdown     = BooleanSetting("-shutdown", "Shutdown compile server")
  val server       = StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "")
  val port         = IntSetting    ("-port",     "Search and start compile server in given port only",
  		                                      0, Some((0, Int.MaxValue)), (_: String) => None)
  val preferIPv4   = BooleanSetting("-ipv4",     "Use IPv4 rather than IPv6 for the server socket")
  val idleMins     = IntSetting    ("-max-idle", "Set idle timeout in minutes for fsc (use 0 for no timeout)",
                                              30, Some((0, Int.MaxValue)), (_: String) => None)

  // For improved help output, separating fsc options from the others.
  def fscSpecific = Set[Settings#Setting](
    currentDir, reset, shutdown, server, port, preferIPv4, idleMins
  )
  val isFscSpecific: String => Boolean = fscSpecific map (_.name)

  /** If a setting (other than a PathSetting) represents a path or paths.
   *  For use in absolutization.
   */
  private def holdsPath = Set[Settings#Setting](d, dependencyfile, pluginsDir)

  override def processArguments(arguments: List[String], processAll: Boolean): (Boolean, List[String]) = {
    val (r, args) = super.processArguments(arguments, processAll)
    // we need to ensure the files specified with relative locations are absolutized based on the currentDir
    (r, args map {a => absolutizePath(a)})
  }

  /**
   * Take an individual path and if it's not absolute turns it into an absolute path based on currentDir.
   * If it's already absolute then it's left alone.
   */
  private[this] def absolutizePath(p: String) = (Path(currentDir.value) resolve Path(p)).normalize.path

  /** All user set settings rewritten with absolute paths based on currentDir */
  def absolutize() {
    userSetSettings foreach {
      case p: OutputSetting => p.outputDirs setSingleOutput AbstractFile.getDirectory(absolutizePath(p.value))
      case p: PathSetting   => p.value = ClassPath.map(p.value, absolutizePath)
      case p: StringSetting => if (holdsPath(p)) p.value = absolutizePath(p.value)
      case _                => ()
    }
  }
}
