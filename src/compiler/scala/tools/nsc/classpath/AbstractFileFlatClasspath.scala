/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FlatClasspath._
import FileUtils.AbstractFileOps
import scala.Some

/**
 * AbstractFile-backed implementation of a classpath.
 */
abstract class AbstractFileFlatClasspath(file: AbstractFile) extends FlatClasspath {

  import AbstractFileFlatClasspath._

  override def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val packagesDirs = dirForPackage.toList.flatMap(_.iterator.filter(file => file.isPackage))
    val prefix = if (inPackage == RootPackage) "" else inPackage + "."

    packagesDirs map { file =>
      PackageEntryImpl(prefix + file.name)
    }
  }

  override def classes(inPackage: String): Seq[ClassfileEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val classfiles = dirForPackage.toList.flatMap(_.iterator.filter(file => file.isClass))

    classfiles map ClassfileEntryImpl
  }

  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassfileEntry]) =
    (packages(inPackage), classes(inPackage))

  override def findClassFile(className: String): Option[AbstractFile] = {
	  val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    classes(pkg).find(_.name == simpleClassName).map(_.file)
  }

	private def getDirectory(forPackage: String): Option[AbstractFile] = {
		if (forPackage == RootPackage) Some(file)
		else {
			val directoryPath = FileUtils.dirPath(forPackage)
			// lookupName might return null but Option.apply will turn it into None
			Option(file.lookupPathUnchecked(directoryPath, directory = true))
		}
	}

}

object AbstractFileFlatClasspath {

  private case class ClassfileEntryImpl(file: AbstractFile) extends ClassfileEntry {
    def name = {
      val className = FileUtils.stripClassExtension(file.name)
      className
    }
  }

}
