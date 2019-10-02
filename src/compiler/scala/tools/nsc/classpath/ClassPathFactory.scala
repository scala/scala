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

import java.net.URL

import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.{CloseableRegistry, Settings}
import FileUtils.AbstractFileOps
import scala.tools.nsc.classpath.ClassPathElement.{DirectoryClassPathElement, PathBasedClassPathElement, ZipJarClassPathElement}
import scala.tools.nsc.util.ClassPath
import scala.tools.util.{PathResolverCaching, PathResolverNoCaching}

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
  def newClassPath(file: AbstractFile, pathResolverCaching: PathResolverCaching): ClassPath = ClassPathFactory.newClassPath(file, settings, closeableRegistry, pathResolverCaching)
  /**
    * Create a new classpath based on the abstract file.
    */
  def newClassPath(file: PathBasedClassPathElement, pathResolverCaching: PathResolverCaching): ClassPath = ClassPathFactory.newClassPathSafe(file, settings, closeableRegistry, pathResolverCaching)

  /**
    * Creators for sub classpaths which preserve this context.
    */
  def sourcesInPath(path: String, pathResolverCaching: PathResolverCaching, ignored: Set[URL]): List[ClassPath] =
     expandPath(path, expandStar = false) collect {case dir:DirectoryClassPathElement if !ignored.contains(dir.url) => createSourcePath(dir, pathResolverCaching)}

  def expandPath(path: String, expandStar: Boolean = true): List[PathBasedClassPathElement] = scala.tools.nsc.util.ClassPath.expandPath(path, expandStar)

  def expandDir(extdir: PathBasedClassPathElement): List[PathBasedClassPathElement] = scala.tools.nsc.util.ClassPath.expandDir(extdir)

  def contentsOfDirsInPath(path: String, pathResolverCaching: PathResolverCaching, ignored: Set[URL]): List[ClassPath] =
    for {
      dir <- expandPath(path, expandStar = false)
      content <- expandDir(dir)
      if (!ignored.contains (content.url))
    } yield newClassPath(content, pathResolverCaching)

  def classesInExpandedPath(path: String, pathResolverCaching: PathResolverCaching, ignored: Set[URL]) =
    classesInPathImpl(path, expand = true, pathResolverCaching, ignored)

  def classesInPath(path: String, pathResolverCaching: PathResolverCaching, ignored: Set[URL]) =
    classesInPathImpl(path, expand = false, pathResolverCaching, ignored)

  def classesInManifest(useManifestClassPath: Boolean, pathResolverCaching: PathResolverCaching, ignored: Set[URL]) =
    if (useManifestClassPath) scala.tools.nsc.util.ClassPath.manifests.collect {
      case url if !ignored.contains(url) => newClassPath(AbstractFile getResources url, pathResolverCaching)
    }
    else Nil

  // Internal
  protected def classesInPathImpl(path: String, expand: Boolean, pathResolverCaching: PathResolverCaching, ignored: Set[URL]) =
    expandPath(path, expand).collect { case path: PathBasedClassPathElement if !ignored.contains (path.url) => newClassPath(path, pathResolverCaching)}


  private def createSourcePath(file: PathBasedClassPathElement, pathResolverCaching: PathResolverCaching): ClassPath =
    file match {
      case jar: ZipJarClassPathElement =>  ZipAndJarSourcePathFactory.create(jar, settings, closeableRegistry, pathResolverCaching)
      case dir: DirectoryClassPathElement =>  DirectorySourcePath(dir.file)
      case _  => sys.error(s"Unsupported sourcepath element: $file")
    }
}

object ClassPathFactory {
  @deprecated("for bincompat in 2.12.x series", "2.12.9")  // TODO remove from 2.13.x
  def newClassPath(file: AbstractFile, settings: Settings): ClassPath =
    newClassPath(file, settings, new CloseableRegistry, PathResolverNoCaching)

  def newClassPath(file: AbstractFile, settings: Settings, closeableRegistry: CloseableRegistry, pathResolverCaching: PathResolverCaching): ClassPath = file match {
    case vd: VirtualDirectory => VirtualDirectoryClassPath(vd)
    case _ =>
      if (file.isJarOrZip)
        ZipAndJarClassPathFactory.create(file, settings, closeableRegistry, pathResolverCaching)
      else if (file.isDirectory)
        DirectoryClassPath(file.file)
      else
        sys.error(s"Unsupported classpath element: $file")
  }
  import ClassPathElement._
  def newClassPath(element: ClassPathElement, settings: Settings, closeableRegistry: CloseableRegistry, pathResolverCaching: PathResolverCaching): ClassPath = element match {
    case vd: VirtualDirectoryClassPathElement => VirtualDirectoryClassPath(vd.dir)
    case zip: ZipJarClassPathElement =>
      ZipAndJarClassPathFactory.create(zip, settings, closeableRegistry, pathResolverCaching)
    case dir: DirectoryClassPathElement =>
      //TODO allow this to be cached  - e.g. for CI
      DirectoryClassPath(dir.file)
    case _ =>
      sys.error(s"Unsupported classpath element: $element")
  }
  def newClassPathSafe(element: PathBasedClassPathElement, settings: Settings, closeableRegistry: CloseableRegistry, pathResolverCaching: PathResolverCaching): ClassPath = element match {
    case zip: ZipJarClassPathElement =>
      ZipAndJarClassPathFactory.create(zip, settings, closeableRegistry, pathResolverCaching)
    case dir: DirectoryClassPathElement =>
      //TODO allow this to be cached  - e.g. for CI
      DirectoryClassPath(dir.file)
  }
}
