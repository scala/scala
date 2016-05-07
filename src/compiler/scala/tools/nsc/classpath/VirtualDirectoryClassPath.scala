package scala.tools.nsc.classpath

import scala.tools.nsc.util.ClassRepresentation
import scala.reflect.io.{Path, PlainFile, VirtualDirectory, AbstractFile}
import FileUtils._
import java.net.URL
import scala.tools.nsc.util.ClassPath

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  type F = AbstractFile

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(dir.lookupName(packageDirName, directory = true))
  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URL(dir.name))
  def asClassPathStrings: Seq[String] = Seq(dir.path)

  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val classFile = new PlainFile(Path(s"$dir/$relativePath.class"))
    if (classFile.exists) Some(classFile)
    else None
  }

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: AbstractFile): Boolean = f.isClass
}
