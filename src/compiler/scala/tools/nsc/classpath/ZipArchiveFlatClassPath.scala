/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.collection.Seq
import scala.reflect.io.AbstractFile
import java.io.File
import scala.reflect.io.FileZipArchive
import java.net.URL
import scala.tools.nsc.Settings

case class ZipArchiveFlatClassPath private(zipFile: File)
  extends FlatClassPath
  with NoSourcePaths {

  import ZipArchiveFlatClassPath._

  val archive = new FileZipArchive(zipFile)

  override def packages(inPackage: String): Seq[PackageEntry] = list(inPackage)._1

  override def classes(inPackage: String): Seq[FileEntry] = list(inPackage)._2

  override def list(inPackage: String): (Seq[PackageEntry], Seq[FileEntry]) = {
    val dirName = s"${FileUtils.dirPath(inPackage)}/"
    val dirEntry = archive.allDirs.getOrElse(dirName, null)

    if (dirEntry == null)
      return (Seq.empty, Seq.empty)

    val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val classFileBuf = collection.mutable.ArrayBuffer.empty[ClassFileEntry]
    val prefix = if (inPackage == FlatClassPath.RootPackage) "" else inPackage + "."
    dirEntry.iterator foreach { entry =>
      if (entry.isDirectory) {
        pkgBuf += PackageEntryImpl(prefix + entry.name)
      } else {
        classFileBuf += ClassFileEntryImpl(entry)
      }
    }
    (pkgBuf, classFileBuf)
  }

  // FIXME implement this
  override def findClassFile(name: String): Option[AbstractFile] = ???

  // FIXME change Nil to real implementation
  override def asURLs: Seq[URL] = Seq(zipFile.toURI.toURL)

  override def asClassPathStrings: Seq[String] = Seq(zipFile.getPath)
}

object ZipArchiveFlatClassPath {

  private case class ClassFileEntryImpl(entry: FileZipArchive#Entry) extends ClassFileEntry {
    override def file: AbstractFile = entry
  }

  private val cache: collection.mutable.Map[File, ZipArchiveFlatClassPath] =
    collection.mutable.Map.empty[File, ZipArchiveFlatClassPath]

  def create(zipFile: File, settings: Settings) = {
    if (settings.YflatCpCaching) createUsingCache(zipFile, settings)
    else new ZipArchiveFlatClassPath(zipFile)
  }

  private def createUsingCache(zipFile: File, settings: Settings) = {
    def newArchive = {
      if (settings.verbose || settings.Ylogcp)
      // TODO maybe use some logger instead of println?
        println(s"Missed cache for $zipFile")
      new ZipArchiveFlatClassPath(zipFile)
    }
    cache.getOrElseUpdate(zipFile, newArchive)
  }
}
