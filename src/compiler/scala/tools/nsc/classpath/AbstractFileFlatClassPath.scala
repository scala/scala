/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URL
import scala.reflect.io.AbstractFile
import scala.tools.nsc.classpath.FlatClassPath.RootPackage
import FileUtils.AbstractFileOps

/**
 * AbstractFile-backed implementation of a classpath.
 */
trait AbstractFileFileLookup[FileEntryType <: FileEntry] extends FlatClassPath {
  // FIXME check whether this class works at all!
  protected val file: AbstractFile

  override def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val packagesDirs = dirForPackage.toList.flatMap(_.iterator.filter(file => file.isPackage))
    val prefix = PackageNameUtils.packagePrefix(inPackage)

    packagesDirs map { file =>
      PackageEntryImpl(prefix + file.name)
    }
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files = dirForPackage.toList.flatMap(_.iterator filter isRequiredFileType)

    files map createFileEntry
  }

  override def list(inPackage: String): FlatClassPathEntries =
    FlatClassPathEntries(packages(inPackage), files(inPackage))

  override def asURLs: Seq[URL] = file.toURLs()

  override def asClassPathStrings: Seq[String] = Seq(file.path)

  private def getDirectory(forPackage: String): Option[AbstractFile] = {
    if (forPackage == RootPackage) Some(file)
    else {
      val directoryPath = FileUtils.dirPath(forPackage)
      // lookupName might return null but Option.apply will turn it into None
      Option(file.lookupPathUnchecked(directoryPath, directory = true))
    }
  }

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
}

import AbstractFileFlatClassPath.ClassFileEntryImpl
trait AbstractFileFlatClassPath
  extends AbstractFileFileLookup[ClassFileEntryImpl]
  with NoSourcePaths {

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    classes(pkg).find(_.name == simpleClassName).map(_.file)
  }

  override def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  override protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
}

object AbstractFileFlatClassPath {

  private[classpath] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry
}

trait AbstractFileFlatSourcePath
  extends AbstractFileFileLookup[SourceFileEntryImpl]
  with NoClassPaths {

  override def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

  override def asSourcePathString: String = asClassPathString

  override protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
}
