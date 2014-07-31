/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.collection.Seq
import scala.reflect.io.AbstractFile
import java.io.File
import scala.reflect.io.FileZipArchive
import java.net.URL
import scala.tools.nsc.Settings

case class ZipArchiveFlatClassPath private(zipFile: File) extends FlatClassPath {

  import ZipArchiveFlatClassPath._

  val archive = new FileZipArchive(zipFile)

  override def packages(inPackage: String): Seq[PackageEntry] = list(inPackage)._1

  override def classes(inPackage: String): Seq[ClassFileEntry] = list(inPackage)._2

  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) = {
    val dirName = s"${FileUtils.dirPath(inPackage)}/"
    val dirEntry = archive.allDirs.getOrElse(dirName, null)

    if (dirEntry == null)
      return (Seq.empty, Seq.empty)

    val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val classfileBuf = collection.mutable.ArrayBuffer.empty[ClassFileEntry]
    val prefix = if (inPackage == FlatClassPath.RootPackage) "" else inPackage + "."
    dirEntry.iterator foreach { entry =>
      if (entry.isDirectory) {
        pkgBuf += PackageEntryImpl(prefix + entry.name)
      } else {
        classfileBuf += ClassFileEntryImpl(entry)
      }
    }
    (pkgBuf, classfileBuf)
  }

  // FIXME implement this
  override def findClassFile(name: String): Option[AbstractFile] = ???

  // FIXME change Nil to real implementation
  override def asURLs: Seq[URL] = Nil
}

object ZipArchiveFlatClassPath {

  private case class ClassFileEntryImpl(entry: FileZipArchive#Entry) extends ClassFileEntry {
    override def name = {
      val className = FileUtils.stripClassExtension(file.name)
      className
    }

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
