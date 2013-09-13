package scala.tools.nsc.classpath

import scala.collection.Seq
import scala.reflect.io.AbstractFile
import java.io.File
import scala.collection.JavaConverters._
import scala.reflect.io.FileZipArchive

case class ZipArchiveFlatClasspath private(zipFile: File) extends FlatClasspath {
  import ZipArchiveFlatClasspath._
  val archive = new FileZipArchive(zipFile)
  import archive.DirEntry

  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')

  def packages(inPackage: String): Seq[PackageEntry] = {
    list(inPackage)._1
  }
  def validClassFile(s: String): Boolean = s.endsWith(".class")
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    list(inPackage)._2
  }

  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassfileEntry]) = {
    val dirName = inPackage.replace('.', '/') + "/"
    val dirEntry = archive.allDirs.getOrElse(dirName, null)
    if (dirEntry == null) return (Seq.empty, Seq.empty)
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

  def findClassFile(name: String): Option[AbstractFile] = ??? 

}

object ZipArchiveFlatClasspath {
  private case class ClassfileEntryImpl(entry: FileZipArchive#Entry) extends ClassfileEntry {
    def name = {
      def stripClassExtension(s: String): String = s.substring(0, s.length-6) // ".class".length == 6
      val className = stripClassExtension(file.name)
      className
    }
    def file: AbstractFile = entry
  }
  private val cache: collection.mutable.Map[File, ZipArchiveFlatClasspath] = 
    collection.mutable.Map.empty[File, ZipArchiveFlatClasspath] 

  def create(zipFile: File) = {
    def newArchive = {
      println(s"Missed cache for $zipFile")
      new ZipArchiveFlatClasspath(zipFile)
    }
    cache.getOrElseUpdate(zipFile, newArchive)
  }
}
