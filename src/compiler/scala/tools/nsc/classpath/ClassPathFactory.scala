/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.Settings
import FileUtils.AbstractFileOps
import scala.tools.nsc.util.ClassPath

/**
 * Provides factory methods for classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class ClassPathFactory(settings: Settings) {
  /**
    * Create a new classpath based on the abstract file.
    */
  def newClassPath(file: AbstractFile): ClassPath = ClassPathFactory.newClassPath(file, settings)

  /**
    * Creators for sub classpaths which preserve this context.
    */
  def sourcesInPath(path: String): List[ClassPath] =
    for {
      file <- expandPath(path, expandStar = false)
      dir <- Option(AbstractFile getDirectory file)
    } yield createSourcePath(dir)


  def expandPath(path: String, expandStar: Boolean = true): List[String] = scala.tools.nsc.util.ClassPath.expandPath(path, expandStar)

  def expandDir(extdir: String): List[String] = scala.tools.nsc.util.ClassPath.expandDir(extdir)

  def contentsOfDirsInPath(path: String): List[ClassPath] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(AbstractFile.getDirectory(name))
    } yield newClassPath(entry)

  def classesInExpandedPath(path: String): IndexedSeq[ClassPath] =
    classesInPathImpl(path, expand = true).toIndexedSeq

  def classesInPath(path: String) = classesInPathImpl(path, expand = false)

  def classesInManifest(useManifestClassPath: Boolean) =
    if (useManifestClassPath) scala.tools.nsc.util.ClassPath.manifests.map(url => newClassPath(AbstractFile getResources url))
    else Nil

  // Internal
  protected def classesInPathImpl(path: String, expand: Boolean) =
    for {
      file <- expandPath(path, expand)
      dir <- Option(AbstractFile.getDirectory(file))
    } yield newClassPath(dir)

  private def createSourcePath(file: AbstractFile): ClassPath =
    if (file.isJarOrZip)
      ZipAndJarSourcePathFactory.create(file, settings)
    else if (file.isDirectory)
      new DirectorySourcePath(file.file)
    else
      sys.error(s"Unsupported sourcepath element: $file")
}

object ClassPathFactory {
  def newClassPath(file: AbstractFile, settings: Settings): ClassPath = file match {
    case vd: VirtualDirectory => VirtualDirectoryClassPath(vd)
    case _ =>
      if (file.isJarOrZip)
        ZipAndJarClassPathFactory.create(file, settings)
      else if (file.isDirectory)
        new DirectoryClassPath(file.file)
      else
        sys.error(s"Unsupported classpath element: $file")
  }
}
