package scala.tools.nsc.classpath

import scala.collection.Seq
import scala.reflect.io.AbstractFile
import java.io.File
import scala.tools.nsc.classpath.RelativePath.RelativeDirectory
import scala.tools.nsc.classpath.ZipFileIndex.Entry
import scala.collection.JavaConverters._

case class ZipFileIndexFlatClasspath(zipFile: File) extends FlatClasspath {
  import ZipFileIndexFlatClasspath._
  val index = ZipFileIndex.getZipFileIndex(zipFile, null, false, null, false)
  val rootDir = new RelativePath.RelativeDirectory("")
  
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')
  
  def packages(inPackage: String): Seq[PackageEntry] = {
    val dirs = if (inPackage == FlatClasspath.RootPackage) {
        val allDirs = index.getAllDirectories().asScala.toArray
        def isTopLevelDir(dir: RelativeDirectory): Boolean = {
          val path = dir.getPath 
          (path.length > 0) && (path.indexOf('/') == (path.length-1))
        }
        def getPackageName(dir: RelativeDirectory): String = {
          val path = dir.getPath
          val pkgName = path.substring(0, path.length-1)
          pkgName
        }
        allDirs.filter(isTopLevelDir).map(getPackageName)
      } else {
        val inPackageDir = RelativePath.RelativeDirectory.forPackage(inPackage)
        index.getDirectories(inPackageDir).asScala.toArray
      }
      val packageDirs = dirs.filter(validPackage)
      val prefix = if (inPackage == FlatClasspath.RootPackage) "" else inPackage + "."
      val foundPackages = packageDirs.map(dirName => PackageEntryImpl(prefix + dirName))
      foundPackages
  }
  def validClassFile(s: String): Boolean = s.endsWith(".class")
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    val inPackageDir = RelativePath.RelativeDirectory.forPackage(inPackage)
    val files = index.getFiles(inPackageDir).asScala.toArray
    val classFiles = files.filter(validClassFile)
    val classfileEntries = classFiles map { classFile =>
      val relativeFile = new RelativePath.RelativeFile(inPackageDir, classFile)
      val entry = index.getZipIndexEntry(relativeFile)
      ClassfileEntryImpl(index, entry)
    }
    classfileEntries
  }
  
  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassfileEntry]) = {
    (packages(inPackage), classes(inPackage))
  }

  def findClassFile(name: String): Option[AbstractFile] = ??? 

}

object ZipFileIndexFlatClasspath {
  private case class ClassfileEntryImpl(index: ZipFileIndex, entry: Entry) extends ClassfileEntry {
    def name = {
      def stripClassExtension(s: String): String = s.substring(0, s.length-6) // ".class".length == 6
      val className = stripClassExtension(file.name)
      className
    }
    def file: AbstractFile = new ZipFileIndexEntryAbstractFile(index, entry)
  }
}
