/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.util.ClassPath
import FileUtils.AbstractFileOps
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.Settings

class FlatClassPathFactory(settings: Settings) extends ClassPathFactory[FlatClassPath] {

  override def expandPath(path: String, expandStar: Boolean = true): List[String] =
    ClassPath.expandPath(path, expandStar)

  override def expandDir(extdir: String): List[String] = ClassPath.expandDir(extdir)

  override def createClassPath(file: AbstractFile): FlatClassPath =
    if (file.isJarOrZip)
      ZipArchiveFlatClassPath.create(file.file, settings)
    else if (file.isDirectory)
      new DirectoryFlatClassPath(file.file)
    else
      sys.error(s"Unsupported classpath element: $file")

  override def sourcesInPath(path: String): List[FlatClassPath] = { // TODO some attempts and tests
    for {
      file <- expandPath(path, expandStar = false)
      dir <- Option(AbstractFile getDirectory file)
    } yield new FlatSourcePath(dir)
  }
}
