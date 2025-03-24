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

package scala.tools.nsc.util

import java.io.{BufferedOutputStream, FileOutputStream}
import java.util.jar.{JarOutputStream, Manifest}

import scala.reflect.io.AbstractFile

trait JarFactory {
  def createJarOutputStream(file: AbstractFile, manifest: Manifest): JarOutputStream
}
final class DefaultJarFactory extends JarFactory{
  override def createJarOutputStream(file: AbstractFile, manifest: Manifest): JarOutputStream =
    new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
}
