/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.settings

import scala.reflect.io.{AbstractFile, PlainFile}

/** Converts paths provided in compiler options (e.g elements of `-classpath` or the target directory of `-d`) to
 *  an `AbstractFile`. */
trait PathFactory {
  def getDirectory(path: String): AbstractFile
  def getFile(path: String): AbstractFile
}

object DefaultPathFactory extends PathFactory {
  override def getDirectory(path: String): AbstractFile = AbstractFile.getDirectory(path)
  override def getFile(path: String): AbstractFile = new PlainFile(path)
}
