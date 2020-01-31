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

package scala.tools.nsc.classpath

import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.{CloseableRegistry, Settings}
import FileUtils.AbstractFileOps
import scala.tools.nsc.util.ClassPath

/**
 * Provides factory methods for classpath. When creating classpath instances for a given path,
 * it uses proper type of classpath depending on a types of particular files containing sources or classes.
 */
class ClassPathFactory(settings: Settings, closeableRegistry: CloseableRegistry = new CloseableRegistry) {

  @deprecated("for bincompat in 2.12.x series", "2.12.9")  // TODO remove from 2.13.x
  def this(settings: Settings) = this(settings, new CloseableRegistry)

  /**
    * Create a new classpath based on the abstract file.
    */
  def newClassPath(file: AbstractFile): ClassPath = ClassPathFactory.newClassPath(file, settings, closeableRegistry)

  /**
    * Creators for sub classpaths which preserve this context.
    */
  def sourcesInPath(path: String): List[ClassPath] =
    for {
      file <- expandPath(path, expandStar = false)
      dir <- Option(settings.pathFactory.getDirectory(file))
    } yield createSourcePath(dir)


  def expandPath(path: String, expandStar: Boolean = true): List[String] = scala.tools.nsc.util.ClassPath.expandPath(path, expandStar)

  def expandDir(extdir: String): List[String] = scala.tools.nsc.util.ClassPath.expandDir(extdir)

  def contentsOfDirsInPath(path: String): List[ClassPath] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(settings.pathFactory.getDirectory(name))
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
      dir <- {
        def asImage = if (file.endsWith(".jimage")) Some(settings.pathFactory.getFile(file)) else None
        Option(settings.pathFactory.getDirectory(file)).orElse(asImage)
      }
    } yield newClassPath(dir)

  private def createSourcePath(file: AbstractFile): ClassPath =
    if (file.isJarOrZip)
      ZipAndJarSourcePathFactory.create(file, settings, closeableRegistry)
    else if (file.isDirectory)
      DirectorySourcePath(file.file)
    else
      sys.error(s"Unsupported sourcepath element: $file")
}

object ClassPathFactory {
  @deprecated("for bincompat in 2.12.x series", "2.12.9")  // TODO remove from 2.13.x
  def newClassPath(file: AbstractFile, settings: Settings): ClassPath =
    newClassPath(file, settings, new CloseableRegistry)
  def newClassPath(file: AbstractFile, settings: Settings, closeableRegistry: CloseableRegistry = new CloseableRegistry): ClassPath = file match {
    case vd: VirtualDirectory => VirtualDirectoryClassPath(vd)
    case _ =>
      if (file.isJarOrZip)
        ZipAndJarClassPathFactory.create(file, settings, closeableRegistry)
      else if (file.isDirectory)
        DirectoryClassPath(file.file)
      else
        sys.error(s"Unsupported classpath element: $file")
  }
}
