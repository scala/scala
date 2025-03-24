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

package scala.tools.nsc.settings

import scala.reflect.io.{AbstractFile, PlainFile}

/** Converts paths provided in compiler options (e.g elements of `-classpath` or the target directory of `-d`) to
 *  an `AbstractFile`. */
trait PathFactory {
  /**
   * Convert the given path into an `AbstractFile` representing an existing directory-like structure, such as a
   * directory on disk, a JAR, or a `VirtualDirectory`. Calling this method will _not_ create the directory
   * if it is absent.
   *
   * @param path The string representing the directory-like path
   * @return the `AbstractFile`, or `null` if the path does not exist or does not represent a directory.
   */
  def getDirectory(path: String): AbstractFile

  /**
   * Convert the given path into an `AbstractFile` representing an file.
   *
   * @param path The string representing the file path
   * @return the `AbstractFile`
   */
  def getFile(path: String): AbstractFile
}

object DefaultPathFactory extends PathFactory {
  override def getDirectory(path: String): AbstractFile = AbstractFile.getDirectory(path)
  override def getFile(path: String): AbstractFile = new PlainFile(path)
}
