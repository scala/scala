/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.net.URL
import scala.reflect.io.AbstractFile
import scala.reflect.io.PlainFile
import scala.tools.nsc.util.ClassRepresentation
import FileUtils._

/**
 * A trait allowing to look for classpath entries in directories. It provides common logic for
 * classes handling class and source files.
 * It makes use of the fact that in the case of nested directories it's easy to find a file
 * when we have a name of a package.
 * It abstracts over the file representation to work with both JFile and AbstractFile.
 */
trait DirectoryLookup[FileEntryType <: ClassRepClassPathEntry] extends FlatClassPath {
  type F

  val dir: F

  protected def emptyFiles: Array[F] // avoids reifying ClassTag[F]
  protected def getSubDir(dirName: String): Option[F]
  protected def listChildren(dir: F, filter: Option[F => Boolean] = None): Array[F]
  protected def getName(f: F): String
  protected def toAbstractFile(f: F): AbstractFile
  protected def isPackage(f: F): Boolean

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isMatchingFile(f: F): Boolean

  private def getDirectory(forPackage: String): Option[F] = {
    if (forPackage == FlatClassPath.RootPackage) {
      Some(dir)
    } else {
      val packageDirName = FileUtils.dirPath(forPackage)
      getSubDir(packageDirName)
    }
  }

  private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isPackage))
    }
    val prefix = PackageNameUtils.packagePrefix(inPackage)
    nestedDirs.map(f => PackageEntryImpl(prefix + getName(f)))
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isMatchingFile))
    }
    files.map(f => createFileEntry(toAbstractFile(f)))
  }

  private[nsc] def list(inPackage: String): FlatClassPathEntries = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory)
    }
    val packagePrefix = PackageNameUtils.packagePrefix(inPackage)
    val packageBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val fileBuf = collection.mutable.ArrayBuffer.empty[FileEntryType]
    for (file <- files) {
      if (isPackage(file))
        packageBuf += PackageEntryImpl(packagePrefix + getName(file))
      else if (isMatchingFile(file))
        fileBuf += createFileEntry(toAbstractFile(file))
    }
    FlatClassPathEntries(packageBuf, fileBuf)
  }
}

trait JFileDirectoryLookup[FileEntryType <: ClassRepClassPathEntry] extends DirectoryLookup[FileEntryType] {
  type F = File

  protected def emptyFiles: Array[File] = Array.empty
  protected def getSubDir(packageDirName: String): Option[File] = {
    val packageDir = new File(dir, packageDirName)
    if (packageDir.exists && packageDir.isDirectory) Some(packageDir)
    else None
  }
  protected def listChildren(dir: File, filter: Option[File => Boolean]): Array[File] = filter match {
    case Some(f) => dir.listFiles(mkFileFilter(f))
    case None => dir.listFiles()
  }
  protected def getName(f: File): String = f.getName
  protected def toAbstractFile(f: File): AbstractFile = new PlainFile(new scala.reflect.io.File(f))
  protected def isPackage(f: File): Boolean = f.isPackage

  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.getPath)
}

case class DirectoryFlatClassPath(dir: File) extends JFileDirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val classFile = new File(s"$dir/$relativePath.class")
    if (classFile.exists) {
      val wrappedClassFile = new scala.reflect.io.File(classFile)
      val abstractClassFile = new PlainFile(wrappedClassFile)
      Some(abstractClassFile)
    } else None
  }

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: File): Boolean = f.isClass

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)
}

case class DirectoryFlatSourcePath(dir: File) extends JFileDirectoryLookup[SourceFileEntryImpl] with NoClassPaths {
  def asSourcePathString: String = asClassPathString

  protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  protected def isMatchingFile(f: File): Boolean = endsScalaOrJava(f.getName)

  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = findSourceFile(className) map SourceFileEntryImpl

  private def findSourceFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val sourceFile = Stream("scala", "java")
      .map(ext => new File(s"$dir/$relativePath.$ext"))
      .collectFirst { case file if file.exists() => file }

    sourceFile.map { file =>
      val wrappedSourceFile = new scala.reflect.io.File(file)
      val abstractSourceFile = new PlainFile(wrappedSourceFile)
      abstractSourceFile
    }
  }

  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)
}
