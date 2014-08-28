/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.io.FileFilter
import java.net.URL
import scala.reflect.io.AbstractFile
import scala.reflect.io.PlainFile
import FileUtils._

trait DirectoryFileLookup[FileEntryType <: ClassRepClassPathEntry] extends FlatClassPath {
  val dir: File
  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  import FlatClassPath.RootPackage
  private def getDirectory(forPackage: String): Option[File] = {
    if (forPackage == RootPackage) {
      Some(dir)
    } else {
      val packageDirName = FileUtils.dirPath(forPackage)
      val packageDir = new File(dir, packageDirName)
      if (packageDir.exists && packageDir.isDirectory) {
        Some(packageDir)
      } else None
    }
  }

  override def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[File] = dirForPackage match {
      case None => Array.empty
      case Some(directory) => directory.listFiles(DirectoryFileLookup.packageDirectoryFileFilter)
    }
    val prefix = PackageNameUtils.packagePrefix(inPackage)
    val entries = nestedDirs map { file =>
      PackageEntryImpl(prefix + file.getName)
    }
    entries
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[File] = dirForPackage match {
      case None => Array.empty
      case Some(directory) => directory.listFiles(fileFilter)
    }
    val entries = files map { file =>
      val wrappedFile = new scala.reflect.io.File(file)
      createFileEntry(new PlainFile(wrappedFile))
    }
    entries
  }

  override def list(inPackage: String): FlatClassPathEntries = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[File] = dirForPackage match {
      case None => Array.empty
      case Some(directory) => directory.listFiles()
    }
    val packagePrefix = PackageNameUtils.packagePrefix(inPackage)
    val packageBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val fileBuf = collection.mutable.ArrayBuffer.empty[FileEntryType]
    for (file <- files) {
      if (file.isPackage) {
        val pkgEntry = PackageEntryImpl(packagePrefix + file.getName)
        packageBuf += pkgEntry
      } else if (fileFilter.accept(file)) {
        val wrappedFile = new scala.reflect.io.File(file)
        val abstractFile = new PlainFile(wrappedFile)
        fileBuf += createFileEntry(abstractFile)
      }
    }
    FlatClassPathEntries(packageBuf, fileBuf)
  }

  override def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
  override def asClassPathStrings: Seq[String] = Seq(dir.getPath)

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def fileFilter: FileFilter
}

object DirectoryFileLookup {

  private[classpath] object packageDirectoryFileFilter extends FileFilter {
    override def accept(pathname: File): Boolean = pathname.isPackage
  }
}

import DirectoryFlatClassPath.ClassFileEntryImpl
case class DirectoryFlatClassPath(dir: File)
  extends DirectoryFileLookup[ClassFileEntryImpl]
  with NoSourcePaths {

  override def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  override def findClassFile(className: String): Option[AbstractFile] = {
    val classFile = new File(dir, s"$className.class")
    if (classFile.exists) {
      val wrappedClassFile = new scala.reflect.io.File(classFile)
      val abstractClassFile = new PlainFile(wrappedClassFile)
      Some(abstractClassFile)
    } else None
  }

  override protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  override protected def fileFilter: FileFilter = DirectoryFlatClassPath.classFileFilter
}

object DirectoryFlatClassPath {

  private[classpath] object classFileFilter extends FileFilter {
    override def accept(pathname: File): Boolean = pathname.isClass
  }

  private[classpath] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry
}

case class DirectoryFlatSourcePath(dir: File)
  extends DirectoryFileLookup[SourceFileEntryImpl]
  with NoClassPaths {

  override def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

  override def asSourcePathString: String = asClassPathString

  override protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  override protected def fileFilter: FileFilter = DirectoryFlatSourcePath.sourceFileFilter
}

object DirectoryFlatSourcePath {

  private[classpath] object sourceFileFilter extends FileFilter {
    override def accept(pathname: File): Boolean = endsScalaOrJava(pathname.getName)
  }
}
