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

case class ZipArchiveFlatClasspath private(zipFile: File) extends FlatClasspath {

  import ZipArchiveFlatClasspath._

  val archive = new FileZipArchive(zipFile)

  override def packages(inPackage: String): Seq[PackageEntry] = list(inPackage)._1

  override def classes(inPackage: String): Seq[ClassfileEntry] = list(inPackage)._2

  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassfileEntry]) = {
    val dirName = s"${FileUtils.dirPath(inPackage)}/"
    val dirEntry = archive.allDirs.getOrElse(dirName, null)

    if (dirEntry == null)
      return (Seq.empty, Seq.empty)

    val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val classfileBuf = collection.mutable.ArrayBuffer.empty[ClassfileEntry]
    val prefix = if (inPackage == FlatClasspath.RootPackage) "" else inPackage + "."
    dirEntry.iterator foreach { entry =>
      if (entry.isDirectory) {
        pkgBuf += PackageEntryImpl(prefix + entry.name)
      } else {
        classfileBuf += ClassfileEntryImpl(entry)
      }
    }
    (pkgBuf, classfileBuf)
  }

  // FIXME implement this
  override def findClassFile(name: String): Option[AbstractFile] = ???

  // FIXME change Nil to real implementation
  override def asURLs: Seq[URL] = Nil
}

object ZipArchiveFlatClasspath {

  private case class ClassfileEntryImpl(entry: FileZipArchive#Entry) extends ClassfileEntry {
    override def name = {
      val className = FileUtils.stripClassExtension(file.name)
      className
    }

    override def file: AbstractFile = entry
  }

  private val cache: collection.mutable.Map[File, ZipArchiveFlatClasspath] =
    collection.mutable.Map.empty[File, ZipArchiveFlatClasspath]

  def create(zipFile: File) = {
    def newArchive = {
      // TODO to remove when debugging won't be needed - or add some flag to force logging
      println(s"Missed cache for $zipFile")
      new ZipArchiveFlatClasspath(zipFile)
    }
    cache.getOrElseUpdate(zipFile, newArchive)
  }
}
