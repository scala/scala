/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
import scala.tools.nsc.util.{ClassRepresentation, EfficientClassPath}

/**
 * A trait allowing to look for classpath entries of given type in zip and jar files.
 * It provides common logic for classes handling class and source files.
 * It's aware of things like e.g. META-INF directory which is correctly skipped.
 */
trait ZipArchiveFileLookup[FileEntryType <: ClassRepresentation] extends EfficientClassPath with Closeable {
  val zipFile: File
  def release: Option[String]

  assert(zipFile != null, "Zip file in ZipArchiveFileLookup cannot be null")

  override def asURLs: Seq[URL] = Seq(zipFile.toURI.toURL)
  override def asClassPathStrings: Seq[String] = Seq(zipFile.getPath)
  private val archive = new FileZipArchive(zipFile, release)
  override def close(): Unit = archive.close()

  override private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if entry.isPackage
    } yield PackageEntryImpl(inPackage.entryName(entry.name))
  }

  protected def files(inPackage: PackageName): Seq[FileEntryType] =
    for {
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if isRequiredFileType(entry, dirEntry.entries.contains)
    } yield createFileEntry(entry)

  protected def file(inPackage: PackageName, name: String): Option[FileEntryType] =
    findDirEntry(inPackage) match {
      case Some(dirEntry) =>
        val entry = dirEntry.lookupName(name, directory = false)
        if (entry != null)
          Some(createFileEntry(entry))
        else
          None
      case _ => None
    }

  override private[nsc] def hasPackage(pkg: PackageName) = findDirEntry(pkg).isDefined

  private[nsc] def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit = {
    findDirEntry(inPackage) match {
      case Some(dirEntry) =>
        for (entry <- dirEntry.iterator) {
          if (entry.isPackage)
            onPackageEntry(PackageEntryImpl(inPackage.entryName(entry.name)))
          else if (isRequiredFileType(entry, dirEntry.entries.contains))
            onClassesAndSources(createFileEntry(entry))
        }
      case None =>
    }
  }

  private def findDirEntry(pkg: PackageName): Option[archive.DirEntry] = {
    Option(archive.allDirs.get(pkg.dirPathTrailingSlash))
  }


  protected def createFileEntry(file: FileZipArchive#Entry): FileEntryType
  protected def isRequiredFileType(file: AbstractFile, siblingExists: String => Boolean): Boolean
}

