/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.fsc

import scala.tools.nsc.Settings
import scala.tools.nsc.util.ClassPath
import scala.reflect.io.{AbstractFile, Path}
import scala.tools.nsc.settings.{DefaultPathFactory, PathFactory}

class FscSettings(error: String => Unit, pathFactory: PathFactory = DefaultPathFactory) extends Settings(error, pathFactory) {
  outer =>

  locally {
    disable(prompt)
    disable(resident)
  }

  val currentDir   = StringSetting ("-current-dir", "path", "Base directory for resolving relative paths", "").internalOnly() withAbbreviation "--current-directory"
  val reset        = BooleanSetting("-reset",    "Reset compile server caches") withAbbreviation "--reset"
  val shutdown     = BooleanSetting("-shutdown", "Shutdown compile server") withAbbreviation "--shutdown"
  val server       = StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "") withAbbreviation "--server"
  val port         = IntSetting    ("-port",     "Search and start compile server in given port only",
                                       0, Some((0, Int.MaxValue)), (_: String) => None) withAbbreviation "--port"
  val preferIPv4   = BooleanSetting("-ipv4",     "Use IPv4 rather than IPv6 for the server socket") withAbbreviation "--ipv4"
  val idleMins     = IntSetting    ("-max-idle", "Set idle timeout in minutes for fsc (use 0 for no timeout)",
                                       30, Some((0, Int.MaxValue)), (_: String) => None) withAbbreviation "--max-idle"

  // For improved help output, separating fsc options from the others.
  def fscSpecific = Set[Settings#Setting](
    currentDir, reset, shutdown, server, port, preferIPv4, idleMins
  )
  val isFscSpecific: String => Boolean = fscSpecific map (_.name)

  /** If a setting (other than a PathSetting) represents a path or paths.
   *  For use in absolutization.
   */
  private def holdsPath = Set[Settings#Setting](outdir, dependencyfile, pluginsDir)

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
  def absolutize(): Unit = {
    userSetSettings foreach {
      case p: OutputSetting => outputDirs.setSingleOutput(AbstractFile.getDirectory(absolutizePath(p.value)))
      case p: PathSetting   => p.value = ClassPath.map(p.value, absolutizePath)
      case p: StringSetting => if (holdsPath(p)) p.value = absolutizePath(p.value)
      case _                => ()
    }
  }
}
