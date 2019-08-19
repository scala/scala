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

import java.io.{Closeable, File}
import java.net.URL

import scala.reflect.io.AbstractFile
import scala.reflect.io.FileZipArchive
import FileUtils.AbstractFileOps
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

/**
 * A trait allowing to look for classpath entries of given type in zip and jar files.
 * It provides common logic for classes handling class and source files.
 * It's aware of things like e.g. META-INF directory which is correctly skipped.
 */
trait ZipArchiveFileLookup[FileEntryType <: ClassRepresentation] extends ClassPath with Closeable {
  val zipFile: File
  def release: Option[String]

  assert(zipFile != null, "Zip file in ZipArchiveFileLookup cannot be null")

  override def asURLs: Seq[URL] = Seq(zipFile.toURI.toURL)
  override def asClassPathStrings: Seq[String] = Seq(zipFile.getPath)
  private val archive = new FileZipArchive(zipFile, release)
  override def close(): Unit = archive.close()

  override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    val prefix = PackageNameUtils.packagePrefix(inPackage)
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if entry.isPackage
    } yield PackageEntryImpl(prefix + entry.name)
  }

  protected def files(inPackage: String): Seq[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if isRequiredFileType(entry)
    } yield createFileEntry(entry)

  protected def file(inPackage: String, name: String): Option[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage)
      entry <- Option(dirEntry.lookupName(name, directory = false))
      if isRequiredFileType(entry)
    } yield createFileEntry(entry)

  override private[nsc] def hasPackage(pkg: String) = findDirEntry(pkg).isDefined
  override private[nsc] def list(inPackage: String): ClassPathEntries = {
    val foundDirEntry = findDirEntry(inPackage)

    foundDirEntry map { dirEntry =>
      val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
      val fileBuf = collection.mutable.ArrayBuffer.empty[FileEntryType]
      val prefix = PackageNameUtils.packagePrefix(inPackage)

      for (entry <- dirEntry.iterator) {
        if (entry.isPackage)
          pkgBuf += PackageEntryImpl(prefix + entry.name)
        else if (isRequiredFileType(entry))
          fileBuf += createFileEntry(entry)
      }
      ClassPathEntries(pkgBuf, fileBuf)
    } getOrElse ClassPathEntries.empty
  }

  private def findDirEntry(pkg: String): Option[archive.DirEntry] = {
    archive.allDirs.get(dottedToPath(pkg))
  }

  private def dottedToPath(dotted: String): String = {
    dotted.replace('.', '/') + "/"
  }

  protected def createFileEntry(file: FileZipArchive#Entry): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
}
