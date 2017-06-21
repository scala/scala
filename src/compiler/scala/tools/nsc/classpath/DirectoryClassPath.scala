/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.net.{URI, URL}
import java.nio.file.{FileSystems, Files, SimpleFileVisitor}
import java.util.function.IntFunction
import java.util
import java.util.Comparator

import scala.reflect.io.{AbstractFile, PlainFile, PlainNioFile}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}
import FileUtils._
import scala.collection.JavaConverters._

/**
 * A trait allowing to look for classpath entries in directories. It provides common logic for
 * classes handling class and source files.
 * It makes use of the fact that in the case of nested directories it's easy to find a file
 * when we have a name of a package.
 * It abstracts over the file representation to work with both JFile and AbstractFile.
 */
trait DirectoryLookup[FileEntryType <: ClassRepresentation] extends ClassPath {
  type F

  val dir: F

  protected def emptyFiles: Array[F] // avoids reifying ClassTag[F]
  protected def getSubDir(dirName: String): Option[F]
  protected def listChildren(dir: F, filter: Option[F => Boolean] = None): Array[F]
  protected def getName(f: F): String
  protected def toAbstractFile(f: F): AbstractFile
  protected def isPackage(f: F): Boolean

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isMatchingFile(f: F): Boolean

  private def getDirectory(forPackage: String): Option[F] = {
    if (forPackage == ClassPath.RootPackage) {
      Some(dir)
    } else {
      val packageDirName = FileUtils.dirPath(forPackage)
      getSubDir(packageDirName)
    }
  }
  override private[nsc] def hasPackage(pkg: String) = getDirectory(pkg).isDefined

  private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isPackage))
    }
    val prefix = PackageNameUtils.packagePrefix(inPackage)
    nestedDirs.map(f => PackageEntryImpl(prefix + getName(f)))
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isMatchingFile))
    }
    files.map(f => createFileEntry(toAbstractFile(f)))
  }

  private[nsc] def list(inPackage: String): ClassPathEntries = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory)
    }
    val packagePrefix = PackageNameUtils.packagePrefix(inPackage)
    val packageBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val fileBuf = collection.mutable.ArrayBuffer.empty[FileEntryType]
    for (file <- files) {
      if (isPackage(file))
        packageBuf += PackageEntryImpl(packagePrefix + getName(file))
      else if (isMatchingFile(file))
        fileBuf += createFileEntry(toAbstractFile(file))
    }
    ClassPathEntries(packageBuf, fileBuf)
  }
}

trait JFileDirectoryLookup[FileEntryType <: ClassRepresentation] extends DirectoryLookup[FileEntryType] {
  type F = File

  protected def emptyFiles: Array[File] = Array.empty
  protected def getSubDir(packageDirName: String): Option[File] = {
    val packageDir = new File(dir, packageDirName)
    if (packageDir.exists && packageDir.isDirectory) Some(packageDir)
    else None
  }
  protected def listChildren(dir: File, filter: Option[File => Boolean]): Array[File] = {
    val listing = filter match {
      case Some(f) => dir.listFiles(mkFileFilter(f))
      case None => dir.listFiles()
    }

    // Sort by file name for stable order of directory .class entries in package scope.
    // This gives stable results ordering of base type sequences for unrelated classes
    // with the same base type depth.
    //
    // Notably, this will stably infer`Product with Serializable`
    // as the type of `case class C(); case class D(); List(C(), D()).head`, rather than the opposite order.
    // On Mac, the HFS performs this sorting transparently, but on Linux the order is unspecified.
    //
    // Note this behaviour can be enabled in javac with `javac -XDsortfiles`, but that's only
    // intended to improve determinism of the compiler for compiler hackers.
    util.Arrays.sort(listing, (o1: File, o2: File) => o1.getName.compareTo(o2.getName))
    listing
  }
  protected def getName(f: File): String = f.getName
  protected def toAbstractFile(f: File): AbstractFile = new PlainFile(new scala.reflect.io.File(f))
  protected def isPackage(f: File): Boolean = f.isPackage

  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.getPath)
}

object JrtClassPath {
  import java.nio.file._, java.net.URI
  def apply(): Option[ClassPath] = {
    try {
      val fs = FileSystems.getFileSystem(URI.create("jrt:/"))
      Some(new JrtClassPath(fs))
    } catch {
      case _: ProviderNotFoundException | _: FileSystemNotFoundException =>
        None
    }
  }
}

/**
  * Implementation `ClassPath` based on the JDK 9 encapsulated runtime modules (JEP-220)
  *
  * https://bugs.openjdk.java.net/browse/JDK-8066492 is the most up to date reference
  * for the structure of the jrt:// filesystem.
  *
  * The implementation assumes that no classes exist in the empty package.
  */
final class JrtClassPath(fs: java.nio.file.FileSystem) extends ClassPath with NoSourcePaths {
  import java.nio.file.Path, java.nio.file._
  type F = Path
  private val dir: Path = fs.getPath("/packages")

  // e.g. "java.lang" -> Seq("/modules/java.base")
  private val packageToModuleBases: Map[String, Seq[Path]] = {
    val ps = Files.newDirectoryStream(dir).iterator().asScala
    def lookup(pack: Path): Seq[Path] = {
      Files.list(pack).iterator().asScala.map(l => if (Files.isSymbolicLink(l)) Files.readSymbolicLink(l) else l).toList
    }
    ps.map(p => (p.toString.stripPrefix("/packages/"), lookup(p))).toMap
  }

  /** Empty string represents root package */
  override private[nsc] def hasPackage(pkg: String) = packageToModuleBases.contains(pkg)
  override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    def matches(packageDottedName: String) =
      if (packageDottedName.contains("."))
        packageOf(packageDottedName) == inPackage
      else inPackage == ""
    packageToModuleBases.keysIterator.filter(matches).map(PackageEntryImpl(_)).toVector
  }
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = {
    if (inPackage == "") Nil
    else {
      packageToModuleBases.getOrElse(inPackage, Nil).flatMap(x =>
        Files.list(x.resolve(inPackage.replace('.', '/'))).iterator().asScala.filter(_.getFileName.toString.endsWith(".class"))).map(x =>
        ClassFileEntryImpl(new PlainNioFile(x))).toVector
    }
  }

  override private[nsc] def list(inPackage: String): ClassPathEntries =
    if (inPackage == "") ClassPathEntries(packages(inPackage), Nil)
    else ClassPathEntries(packages(inPackage), classes(inPackage))

  def asURLs: Seq[URL] = Seq(dir.toUri.toURL)
  // We don't yet have a scheme to represent the JDK modules in our `-classpath`.
  // java models them as entries in the new "module path", we'll probably need to follow this.
  def asClassPathStrings: Seq[String] = Nil

  def findClassFile(className: String): Option[AbstractFile] = {
    if (!className.contains(".")) None
    else {
      val inPackage = packageOf(className)
      packageToModuleBases.getOrElse(inPackage, Nil).iterator.flatMap{x =>
        val file = x.resolve(className.replace('.', '/') + ".class")
        if (Files.exists(file)) new scala.reflect.io.PlainNioFile(file) :: Nil else Nil
      }.take(1).toList.headOption
    }
  }
  private def packageOf(dottedClassName: String): String =
    dottedClassName.substring(0, dottedClassName.lastIndexOf("."))
}

case class DirectoryClassPath(dir: File) extends JFileDirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val classFile = new File(s"$dir/$relativePath.class")
    if (classFile.exists) {
      val wrappedClassFile = new scala.reflect.io.File(classFile)
      val abstractClassFile = new PlainFile(wrappedClassFile)
      Some(abstractClassFile)
    } else None
  }

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: File): Boolean = f.isClass

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)
}

case class DirectorySourcePath(dir: File) extends JFileDirectoryLookup[SourceFileEntryImpl] with NoClassPaths {
  def asSourcePathString: String = asClassPathString

  protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  protected def isMatchingFile(f: File): Boolean = endsScalaOrJava(f.getName)

  override def findClass(className: String): Option[ClassRepresentation] = findSourceFile(className) map SourceFileEntryImpl

  private def findSourceFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val sourceFile = Stream("scala", "java")
      .map(ext => new File(s"$dir/$relativePath.$ext"))
      .collectFirst { case file if file.exists() => file }

    sourceFile.map { file =>
      val wrappedSourceFile = new scala.reflect.io.File(file)
      val abstractSourceFile = new PlainFile(wrappedSourceFile)
      abstractSourceFile
    }
  }

  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)
}
