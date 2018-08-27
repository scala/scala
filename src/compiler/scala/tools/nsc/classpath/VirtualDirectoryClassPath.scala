package scala.tools.nsc.classpath

import java.io.File
import java.net.URL
import java.nio.file.attribute.BasicFileAttributes

import scala.reflect.internal.util.AbstractFileClassLoader
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.tools.nsc.classpath.FileUtils._
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  type F = AbstractFile

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(AbstractFileClassLoader.lookupPath(dir)(packageDirName.split('/'), directory = true))
  override protected def listChildren(dir: AbstractFile)(f: (AbstractFile, BasicFileAttributes) => Boolean): (Array[AbstractFile], Array[AbstractFile]) = {
    val files = new collection.mutable.ArrayBuilder.ofRef[AbstractFile]()
    val dirs = new collection.mutable.ArrayBuilder.ofRef[AbstractFile]()
    for (af <- dir.iterator) {
      if (f(af, af.asNioBasicFileAttributes)) {
        if (af.isDirectory) dirs += af
        else files += af
      }
    }
    (dirs.result(), files.result())
  }

  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile, attrs: BasicFileAttributes): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URL(dir.name))
  def asClassPathStrings: Seq[String] = Seq(dir.path)

  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className) + ".class"
    Option(AbstractFileClassLoader.lookupPath(dir)(relativePath split '/', directory = false))
  }

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: AbstractFile, attrs: BasicFileAttributes): Boolean = f.isClass
}
