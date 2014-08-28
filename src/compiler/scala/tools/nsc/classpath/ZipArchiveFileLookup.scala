/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.net.URL
import scala.collection.Seq
import scala.reflect.io.AbstractFile
import scala.reflect.io.FileZipArchive

trait ZipArchiveFileLookup[FileEntryType <: ClassRepClassPathEntry] extends FlatClassPath {
  val zipFile: File

  assert(zipFile != null, "Zip file in ZipArchiveFileLookup cannot be null")

  private val archive = new FileZipArchive(zipFile)

  override def packages(inPackage: String): Seq[PackageEntry] = listImpl(inPackage)._1

  protected def files(inPackage: String): Seq[FileEntryType] = listImpl(inPackage)._2

  override def list(inPackage: String): FlatClassPathEntries = {
    val (packages, files) = listImpl(inPackage)
    FlatClassPathEntries(packages, files)
  }

  private def listImpl(inPackage: String): (Seq[PackageEntry], Seq[FileEntryType]) = {
    val dirName = s"${FileUtils.dirPath(inPackage)}/"
    val foundDirEntry = archive.allDirs.get(dirName)

    foundDirEntry.map { dirEntry =>
      val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
      val fileBuf = collection.mutable.ArrayBuffer.empty[FileEntryType]
      val prefix = PackageNameUtils.packagePrefix(inPackage)
      dirEntry.iterator foreach {
        entry =>
          if (entry.isDirectory) {
            pkgBuf += PackageEntryImpl(prefix + entry.name)
          } else if (isRequiredFileType(entry)) {
            fileBuf += createFileEntry(entry)
          }
      }
      (pkgBuf, fileBuf)
    } getOrElse (Seq.empty, Seq.empty)
  }

  override def asURLs: Seq[URL] = Seq(zipFile.toURI.toURL)
  override def asClassPathStrings: Seq[String] = Seq(zipFile.getPath)

  protected def createFileEntry(file: FileZipArchive#Entry): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
}
