/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.util.ClassPath
import FileUtils.AbstractFileOps
import scala.tools.nsc.io.AbstractFile

class FlatClasspathFactory extends ClasspathFactory[FlatClasspath] {

  override def expandPath(path: String, expandStar: Boolean = true): List[String] =
    ClassPath.expandPath(path, expandStar)

  override def expandDir(extdir: String): List[String] = ClassPath.expandDir(extdir)

  override def createClassPath(file: AbstractFile): FlatClasspath =
    if (file.isJarOrZip)
      ZipArchiveFlatClasspath.create(file.file)
    else if (file.isDirectory)
      new DirectoryFlatClasspath(file.file)
    else
      sys.error(s"Unsupported classpath element: $file")

  override def sourcesInPath(path: String): List[FlatClasspath] = {
    // TODO: implement properly
    Nil
  }
}
