/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile

/**
 * A trait that contains factory methods for classpath elements of type T.
 *
 * The logic has been abstract from ClassPath#ClassPathContext so it's possible
 * to have common trait that supports both old and new classpath implementation.
 *
 * Therefore, we expect that T will be either ClassPath[U] or FlatClassPath.
 */
trait ClassPathFactory[T] {

  def expandPath(path: String, expandStar: Boolean = true): List[String]

  def expandDir(extdir: String): List[String]

  /**
   * Create a new classpath based on the abstract file.
   */
  def createClassPath(file: AbstractFile): T

  /**
   * Creators for sub classpaths which preserve this context.
   */
  def sourcesInPath(path: String): List[T]

  protected def manifests: List[java.net.URL] = {
    import scala.collection.convert.WrapAsScala.enumerationAsScalaIterator
    Thread.currentThread().getContextClassLoader()
      .getResources("META-INF/MANIFEST.MF")
      .filter(_.getProtocol == "jar").toList
  }

  def contentsOfDirsInPath(path: String): List[T] =
    for {
      dir <- expandPath(path, expandStar = false)
      name <- expandDir(dir)
      entry <- Option(AbstractFile.getDirectory(name))
    } yield createClassPath(entry)

  def classesInExpandedPath(path: String): IndexedSeq[T] =
    classesInPathImpl(path, expand = true).toIndexedSeq

  def classesInPath(path: String) = classesInPathImpl(path, expand = false)

  // Internal
  protected def classesInPathImpl(path: String, expand: Boolean) =
    for {
      file <- expandPath(path, expand)
      dir <- Option(AbstractFile.getDirectory(file))
    } yield createClassPath(dir)

  def classesInManifest(useManifestClassPath: Boolean) =
    if (useManifestClassPath) manifests.map(url => createClassPath(AbstractFile getResources url))
    else Nil
}
