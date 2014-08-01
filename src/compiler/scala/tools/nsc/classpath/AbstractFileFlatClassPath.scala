/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FlatClassPath._
import FileUtils.AbstractFileOps
import scala.Some
import java.net.URL

/**
 * AbstractFile-backed implementation of a classpath.
 */
abstract class AbstractFileFlatClassPath(file: AbstractFile) extends FlatClassPath {

  import AbstractFileFlatClassPath._

  override def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val packagesDirs = dirForPackage.toList.flatMap(_.iterator.filter(file => file.isPackage))
    val prefix = if (inPackage == RootPackage) "" else inPackage + "."

    packagesDirs map { file =>
      PackageEntryImpl(prefix + file.name)
    }
  }

  override def classes(inPackage: String): Seq[ClassFileEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val classfiles = dirForPackage.toList.flatMap(_.iterator.filter(file => file.isClass))

    classfiles map ClassFileEntryImpl
  }

  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) =
    (packages(inPackage), classes(inPackage))

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    classes(pkg).find(_.name == simpleClassName).map(_.file)
  }

  // FIXME implement this
  override def asURLs: Seq[URL] = file.toURLs()

  private def getDirectory(forPackage: String): Option[AbstractFile] = {
    if (forPackage == RootPackage) Some(file)
    else {
      val directoryPath = FileUtils.dirPath(forPackage)
      // lookupName might return null but Option.apply will turn it into None
      Option(file.lookupPathUnchecked(directoryPath, directory = true))
    }
  }

}

object AbstractFileFlatClassPath {

  private case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry {
    def name = {
      val className = FileUtils.stripClassExtension(file.name)
      className
    }
  }

}
