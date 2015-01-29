/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassPath

/**
 * A trait that contains factory methods for classpath elements of type T.
 *
 * The logic has been abstracted from ClassPath#ClassPathContext so it's possible
 * to have common trait that supports both recursive and flat classpath representations.
 *
 * Therefore, we expect that T will be either ClassPath[U] or FlatClassPath.
 */
trait ClassPathFactory[T] {

  /**
   * Create a new classpath based on the abstract file.
   */
  def newClassPath(file: AbstractFile): T

  /**
   * Creators for sub classpaths which preserve this context.
   */
  def sourcesInPath(path: String): List[T]

  def expandPath(path: String, expandStar: Boolean = true): List[String] = ClassPath.expandPath(path, expandStar)

  def expandDir(extdir: String): List[String] = ClassPath.expandDir(extdir)

  def contentsOfDirsInPath(path: String): List[T] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(AbstractFile.getDirectory(name))
    } yield newClassPath(entry)

  def classesInExpandedPath(path: String): IndexedSeq[T] =
    classesInPathImpl(path, expand = true).toIndexedSeq

  def classesInPath(path: String) = classesInPathImpl(path, expand = false)

  def classesInManifest(useManifestClassPath: Boolean) =
    if (useManifestClassPath) ClassPath.manifests.map(url => newClassPath(AbstractFile getResources url))
    else Nil

  // Internal
  protected def classesInPathImpl(path: String, expand: Boolean) =
    for {
      file <- expandPath(path, expand)
      dir <- Option(AbstractFile.getDirectory(file))
    } yield newClassPath(dir)
}
