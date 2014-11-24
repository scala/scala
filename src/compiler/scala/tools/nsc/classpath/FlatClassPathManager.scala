package scala.tools.nsc.classpath

import scala.tools.nsc.Settings

/**
 * Manages creation and (possible) caching of flat classpath instances.
 */
trait FlatClassPathManager {
  def createClassPath(settings: Settings): FlatClassPath
}
