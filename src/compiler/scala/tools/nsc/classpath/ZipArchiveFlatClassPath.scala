package scala.tools.nsc.classpath

import scala.collection.Seq
import scala.reflect.io.AbstractFile
import java.io.File
import scala.reflect.io.FileZipArchive

case class ZipArchiveFlatClassPath private(zipFile: File) extends FlatClassPath {
  import ZipArchiveFlatClassPath._
  val archive = new FileZipArchive(zipFile)

  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')

  def packages(inPackage: String): Seq[PackageEntry] = {
    list(inPackage)._1
  }
  def validClassFile(s: String): Boolean = s.endsWith(".class")
  def classes(inPackage: String): Seq[ClassFileEntry] = {
    list(inPackage)._2
  }

  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) = {
    val dirName = inPackage.replace('.', '/') + "/"
    val dirEntry = archive.allDirs.getOrElse(dirName, null)
    if (dirEntry == null) return (Seq.empty, Seq.empty)
    val pkgBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val classFileBuf = collection.mutable.ArrayBuffer.empty[ClassFileEntry]
    val prefix = if (inPackage == FlatClassPath.RootPackage) "" else inPackage + "."
    dirEntry.iterator foreach { entry =>
      if (entry.isDirectory) {
        pkgBuf += PackageEntryImpl(prefix + entry.name)
      } else {
        classFileBuf += ClassfileEntryImpl(entry)
      }
    }
    (pkgBuf, classFileBuf)
  }

  def findClassFile(name: String): Option[AbstractFile] = ??? 

}

object ZipArchiveFlatClassPath {
  private case class ClassfileEntryImpl(entry: FileZipArchive#Entry) extends ClassFileEntry {
    def name = {
      def stripClassExtension(s: String): String = s.substring(0, s.length-6) // ".class".length == 6
      val className = stripClassExtension(file.name)
      className
    }
    def file: AbstractFile = entry
  }
  private val cache: collection.mutable.Map[File, ZipArchiveFlatClassPath] =
    collection.mutable.Map.empty[File, ZipArchiveFlatClassPath]

  def create(zipFile: File) = {
    def newArchive = {
      println(s"Missed cache for $zipFile")
      new ZipArchiveFlatClassPath(zipFile)
    }
    cache.getOrElseUpdate(zipFile, newArchive)
  }
}
