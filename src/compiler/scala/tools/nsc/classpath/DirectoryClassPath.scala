/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.classpath

import java.io.{Closeable, File}
import java.net.URL
import java.util

import scala.reflect.io.{AbstractFile, PlainFile, PlainNioFile}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation, EfficientClassPath}
import FileUtils._
import scala.collection.JavaConverters._
import scala.reflect.internal.JDK9Reflectors
import scala.tools.nsc.CloseableRegistry
import scala.tools.nsc.classpath.PackageNameUtils.{packageContains, separatePkgAndClassNames}

/**
 * A trait allowing to look for classpath entries in directories. It provides common logic for
 * classes handling class and source files.
 * It makes use of the fact that in the case of nested directories it's easy to find a file
 * when we have a name of a package.
 * It abstracts over the file representation to work with both JFile and AbstractFile.
 */
trait DirectoryLookup[FileEntryType <: ClassRepresentation] extends EfficientClassPath {
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

  private def getDirectory(forPackage: PackageName): Option[F] = {
    if (forPackage.isRoot) {
      Some(dir)
    } else {
      getSubDir(forPackage.dirPathTrailingSlash)
    }
  }
  override private[nsc] def hasPackage(pkg: PackageName) = getDirectory(pkg).isDefined

  private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)

    val nestedDirs: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isPackage))
    }
    nestedDirs.map(f => PackageEntryImpl(inPackage.entryName(getName(f))))
  }

  protected def files(inPackage: PackageName): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isMatchingFile))
    }
    files.map(f => createFileEntry(toAbstractFile(f)))
  }

  override private[nsc] def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit = {
    val dirForPackage = getDirectory(inPackage)
    dirForPackage match {
      case None =>
      case Some(directory) =>
        for (file <- listChildren(directory)) {
          if (isPackage(file))
            onPackageEntry(PackageEntryImpl(inPackage.entryName(getName(file))))
          else if (isMatchingFile(file))
            onClassesAndSources(createFileEntry(toAbstractFile(file)))
        }
    }
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
  private val jrtClassPathCache = new FileBasedCache[Unit, JrtClassPath]()
  private val ctSymClassPathCache = new FileBasedCache[Unit, CtSymClassPath]()
  def apply(release: Option[String], closeableRegistry: CloseableRegistry): Option[ClassPath] = {
    import scala.util.Properties._
    if (!isJavaAtLeast("9")) None
    else {
      // TODO escalate errors once we're sure they are fatal
      // I'm hesitant to do this immediately, because -release will still work for multi-release JARs
      // even if we're running on a JRE or a non OpenJDK JDK where ct.sym is unavailable.
      //
      // Longer term we'd like an official API for this in the JDK
      // Discussion: http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/thread.html#11738

      val currentMajorVersion: Int = JDK9Reflectors.runtimeVersionMajor(JDK9Reflectors.runtimeVersion()).intValue()
      release match {
        case Some(v) if v.toInt < currentMajorVersion =>
          try {
            val ctSym = Paths.get(javaHome).resolve("lib").resolve("ct.sym")
            if (Files.notExists(ctSym)) None
            else {
              val classPath = ctSymClassPathCache.getOrCreate((), ctSym :: Nil, () => new CtSymClassPath(ctSym, v.toInt), closeableRegistry, true)
              Some(classPath)
            }
          } catch {
            case _: Throwable => None
          }
        case _ =>
          try {
            val fs = FileSystems.getFileSystem(URI.create("jrt:/"))
            val classPath = jrtClassPathCache.getOrCreate((), Nil, () => new JrtClassPath(fs), closeableRegistry, false)
            Some(classPath)
          } catch {
            case _: ProviderNotFoundException | _: FileSystemNotFoundException => None
          }
      }
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
  override private[nsc] def hasPackage(pkg: PackageName) = packageToModuleBases.contains(pkg.dottedString)
  override private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    packageToModuleBases.keysIterator.filter(pack => packageContains(inPackage.dottedString, pack)).map(PackageEntryImpl(_)).toVector
  }
  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = {
    if (inPackage.isRoot) Nil
    else {
      packageToModuleBases.getOrElse(inPackage.dottedString, Nil).flatMap(x =>
        Files.list(x.resolve(inPackage.dirPathTrailingSlash)).iterator().asScala.filter(_.getFileName.toString.endsWith(".class"))).map(x =>
        ClassFileEntryImpl(new PlainNioFile(x))).toVector
    }
  }

  override private[nsc] def list(inPackage: PackageName): ClassPathEntries =
    if (inPackage.isRoot) ClassPathEntries(packages(inPackage), Nil)
    else ClassPathEntries(packages(inPackage), classes(inPackage))

  def asURLs: Seq[URL] = Seq(new URL("jrt:/"))
  // We don't yet have a scheme to represent the JDK modules in our `-classpath`.
  // java models them as entries in the new "module path", we'll probably need to follow this.
  def asClassPathStrings: Seq[String] = Nil

  def findClassFile(className: String): Option[AbstractFile] = {
    if (!className.contains(".")) None
    else {
      val (inPackage, _) = separatePkgAndClassNames(className)
      packageToModuleBases.getOrElse(inPackage, Nil).iterator.flatMap { x =>
        val file = x.resolve(className.replace('.', '/') + ".class")
        if (Files.exists(file)) new scala.reflect.io.PlainNioFile(file) :: Nil else Nil
      }.take(1).toList.headOption
    }
  }
}

/**
  * Implementation `ClassPath` based on the \$JAVA_HOME/lib/ct.sym backing http://openjdk.java.net/jeps/247
  */
final class CtSymClassPath(ctSym: java.nio.file.Path, release: Int) extends ClassPath with NoSourcePaths with Closeable {
  import java.nio.file.Path, java.nio.file._

  private val fileSystem: FileSystem = FileSystems.newFileSystem(ctSym, null: ClassLoader)
  private val root: Path = fileSystem.getRootDirectories.iterator().next
  private val roots = Files.newDirectoryStream(root).iterator().asScala.toList

  // http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/011737.html
  private def codeFor(major: Int): String = if (major < 10) major.toString else ('A' + (major - 10)).toChar.toString

  private val releaseCode: String = codeFor(release)
  private def fileNameMatchesRelease(fileName: String) = !fileName.contains("-") && fileName.contains(releaseCode) // exclude `9-modules`
  private val rootsForRelease: List[Path] = roots.filter(root => fileNameMatchesRelease(root.getFileName.toString))

  // e.g. "java.lang" -> Seq(/876/java/lang, /87/java/lang, /8/java/lang))
  private val packageIndex: scala.collection.Map[String, Seq[Path]] = {
    val index = collection.mutable.AnyRefMap[String, collection.mutable.ListBuffer[Path]]()
    val isJava12OrHigher = scala.util.Properties.isJavaAtLeast("12")
    rootsForRelease.foreach(root => Files.walk(root).iterator().asScala.filter(Files.isDirectory(_)).foreach { p =>
      val moduleNamePathElementCount = if (isJava12OrHigher) 1 else 0
      if (p.getNameCount > root.getNameCount + moduleNamePathElementCount) {
        val packageDotted = p.subpath(moduleNamePathElementCount + root.getNameCount, p.getNameCount).toString.replace('/', '.')
        index.getOrElseUpdate(packageDotted, new collection.mutable.ListBuffer) += p
      }
    })
    index
  }

  /** Empty string represents root package */
  override private[nsc] def hasPackage(pkg: PackageName) = packageIndex.contains(pkg.dottedString)
  override private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    packageIndex.keysIterator.filter(pack => packageContains(inPackage.dottedString, pack)).map(PackageEntryImpl(_)).toVector
  }
  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = {
    if (inPackage.isRoot) Nil
    else {
      val sigFiles = packageIndex.getOrElse(inPackage.dottedString, Nil).iterator.flatMap(p =>
        Files.list(p).iterator().asScala.filter(_.getFileName.toString.endsWith(".sig")))
      sigFiles.map(f => ClassFileEntryImpl(new PlainNioFile(f))).toVector
    }
  }

  override private[nsc] def list(inPackage: PackageName): ClassPathEntries =
    if (inPackage.isRoot) ClassPathEntries(packages(inPackage), Nil)
    else ClassPathEntries(packages(inPackage), classes(inPackage))

  def asURLs: Seq[URL] = Nil
  def asClassPathStrings: Seq[String] = Nil
  override def close(): Unit = fileSystem.close()
  def findClassFile(className: String): Option[AbstractFile] = {
    if (!className.contains(".")) None
    else {
      val (inPackage, classSimpleName) = separatePkgAndClassNames(className)
      packageIndex.getOrElse(inPackage, Nil).iterator.flatMap { p =>
        val file = p.resolve(classSimpleName + ".sig")
        if (Files.exists(file)) new scala.reflect.io.PlainNioFile(file) :: Nil else Nil
      }.take(1).toList.headOption
    }
  }
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

  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = files(inPackage)
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

  private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)
}
