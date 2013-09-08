package scala.tools.nsc.util

import scala.tools.nsc.io.AbstractFile

/**
 * Simple interface that allows us to abstract over how class file lookup is performed
 * in different classpath implementations.
 */
trait ClassfileLookup {
  def findClassFile(name: String): Option[AbstractFile]
}