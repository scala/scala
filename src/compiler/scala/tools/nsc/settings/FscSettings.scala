/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package nsc
package settings

import util.ClassPath
import io.{ Directory, Path, AbstractFile }

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
  val preferIPv4   = BooleanSetting("-ipv4",     "Use IPv4 rather than IPv6 for the server socket")
  val idleMins     = IntSetting    ("-max-idle", "Set idle timeout in minutes for fsc (use 0 for no timeout)",
                                              30, Some((0, Int.MaxValue)), (_: String) => None)

  // For improved help output, separating fsc options from the others.
  def fscSpecific = Set[Settings#Setting](
    currentDir, reset, shutdown, server, preferIPv4, idleMins
  )
  val isFscSpecific: String => Boolean = fscSpecific map (_.name)

  /** If a setting (other than a PathSetting) represents a path or paths.
   *  For use in absolutization.
   */
  private def holdsPath = Set[Settings#Setting](
    d, dependencyfile, pluginsDir, Ygenjavap
  )

  /** All user set settings rewritten with absolute paths. */
  def absolutize(root: Path) {
    def rewrite(p: String) = (root resolve Path(p)).normalize.path
    userSetSettings foreach {
      case p: OutputSetting => p.outputDirs setSingleOutput AbstractFile.getDirectory(rewrite(p.value))
      case p: PathSetting   => p.value = ClassPath.map(p.value, rewrite)
      case p: StringSetting => if (holdsPath(p)) p.value = rewrite(p.value)
      case _                => ()
    }
  }
}
