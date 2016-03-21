/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings
import scala.tools.nsc.io.AbstractFile
import FileUtils.AbstractFileOps

/**
 * Provides factory methods for flat classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class FlatClassPathFactory(settings: Settings) extends ClassPathFactory[FlatClassPath] {
  def newClassPath(file: AbstractFile): FlatClassPath = file match {
    case vd: VirtualDirectory => VirtualDirectoryFlatClassPath(vd)
    case _ =>
      if (file.isJarOrZip)
        ZipAndJarFlatClassPathFactory.create(file, settings)
      else if (file.isDirectory)
        new DirectoryFlatClassPath(file.file)
      else
        sys.error(s"Unsupported classpath element: $file")
  }

  def sourcesInPath(path: String): List[FlatClassPath] =
    for {
      file <- expandPath(path, expandStar = false)
      dir <- Option(AbstractFile getDirectory file)
    } yield createSourcePath(dir)

  private def createSourcePath(file: AbstractFile): FlatClassPath =
    if (file.isJarOrZip)
      ZipAndJarFlatSourcePathFactory.create(file, settings)
    else if (file.isDirectory)
      new DirectoryFlatSourcePath(file.file)
    else
      sys.error(s"Unsupported sourcepath element: $file")
}
