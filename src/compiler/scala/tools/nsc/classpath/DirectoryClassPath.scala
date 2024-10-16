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
import java.net.{URI, URL}
import java.nio.file._
import java.util.Collections

import scala.jdk.CollectionConverters._
import scala.reflect.internal.JDK9Reflectors
import scala.reflect.io.{AbstractFile, PlainFile, PlainNioFile}
import scala.tools.nsc.CloseableRegistry
import scala.tools.nsc.classpath.PackageNameUtils.{packageContains, separatePkgAndClassNames}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation, EfficientClassPath}
import scala.util.Properties.{isJavaAtLeast, javaHome}
import scala.util.control.NonFatal
import FileUtils._

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
  protected def hasChild(dir: F, name: String): Boolean
  protected def getName(f: F): String
  protected def toAbstractFile(f: F): AbstractFile
  protected def isPackage(f: F): Boolean

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isMatchingFile(f: F, siblingExists: String => Boolean): Boolean

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
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      nestedDirs.map(f => PackageEntryImpl(inPackage.entryName(getName(f))))
    )
  }

  protected def files(inPackage: PackageName): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) =>
        val hasCh = hasChild(directory, _)
        listChildren(directory, Some(f => isMatchingFile(f, hasCh)))
    }
    files.iterator.map(f => createFileEntry(toAbstractFile(f))).toSeq
  }

  override private[nsc] def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit = {
    val dirForPackage = getDirectory(inPackage)
    dirForPackage match {
      case None =>
      case Some(directory) =>
        val hasCh = hasChild(directory, _)
        for (file <- listChildren(directory)) {
          if (isPackage(file))
            onPackageEntry(PackageEntryImpl(inPackage.entryName(getName(file))))
          else if (isMatchingFile(file, hasCh))
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
    if (packageDir.exists && packageDir.isDirectory && packageDir.canRead) Some(packageDir)
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
    java.util.Arrays.sort(listing, (o1: File, o2: File) => o1.getName.compareTo(o2.getName))
    listing
  }
  protected def hasChild(dir: File, name: String): Boolean = new File(dir, name).isFile
  protected def getName(f: File): String = f.getName
  protected def toAbstractFile(f: File): AbstractFile = new PlainFile(new scala.reflect.io.File(f))
  protected def isPackage(f: File): Boolean = f.isPackage

  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.getPath)
}

object JrtClassPath {
  private val jrtClassPathCache = new FileBasedCache[Option[String], JrtClassPath]()
  private val ctSymClassPathCache = new FileBasedCache[String, CtSymClassPath]()
  def apply(release: Option[String], systemPath: Option[String], unsafe: Option[List[String]], closeableRegistry: CloseableRegistry): List[ClassPath] =
    if (!isJavaAtLeast("9")) Nil
    else {
      // TODO escalate errors once we're sure they are fatal
      // I'm hesitant to do this immediately, because -release will still work for multi-release JARs
      // even if we're running on a JRE or a non OpenJDK JDK where ct.sym is unavailable.
      //
      // Longer term we'd like an official API for this in the JDK
      // Discussion: https://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/thread.html#11738

      val currentMajorVersion: Int = JDK9Reflectors.runtimeVersionMajor(JDK9Reflectors.runtimeVersion()).intValue()
      release match {
        case Some(version) if version.toInt < currentMajorVersion =>
          val ct = createCt(version, closeableRegistry)
          unsafe match {
            case Some(pkgs) if pkgs.nonEmpty =>
              createJrt(systemPath, closeableRegistry) match {
                case Nil  => ct
                case jrts => ct.appended(new FilteringJrtClassPath(jrts.head, pkgs: _*))
              }
            case _ => ct
          }
        case _ =>
          createJrt(systemPath, closeableRegistry)
      }
    }
  private def createCt(v: String, closeableRegistry: CloseableRegistry): List[ClassPath] =
    try {
      val ctSym = Paths.get(javaHome).resolve("lib").resolve("ct.sym")
      if (Files.notExists(ctSym)) Nil
      else {
        val classPath = ctSymClassPathCache.getOrCreate(v, ctSym :: Nil, () => new CtSymClassPath(ctSym, v.toInt), closeableRegistry, checkStamps = true)
        List(classPath)
      }
    } catch {
      case NonFatal(_) => Nil
    }
  private def createJrt(systemPath: Option[String], closeableRegistry: CloseableRegistry): List[JrtClassPath] =
    try {
      val classPath = jrtClassPathCache.getOrCreate(systemPath, Nil, () => {
        val fs = systemPath match {
          case Some(javaHome) => FileSystems.newFileSystem(URI.create("jrt:/"), Collections.singletonMap("java.home", javaHome))
          case None => FileSystems.getFileSystem(URI.create("jrt:/"))
        }
        new JrtClassPath(fs, systemPath.isDefined)
      }, closeableRegistry, checkStamps = false)
      List(classPath)
    } catch {
      case _: ProviderNotFoundException | _: FileSystemNotFoundException => Nil
    }
}

final class FilteringJrtClassPath(delegate: JrtClassPath, allowed: String*) extends ClassPath with NoSourcePaths {
  private val allowedPackages = allowed
  private def packagePrefix(p: String, q: String) = p.startsWith(q) && (p.length == q.length || p.charAt(q.length) == '.')
  private def ok(pkg: PackageName) = pkg.dottedString.isEmpty || allowedPackages.exists(packagePrefix(_, pkg.dottedString))
  def asClassPathStrings: Seq[String] = delegate.asClassPathStrings
  def asURLs: Seq[java.net.URL] = delegate.asURLs
  private[nsc] def classes(inPackage: PackageName) = if (ok(inPackage)) delegate.classes(inPackage) else Nil
  def findClassFile(className: String) = if (ok(PackageName(separatePkgAndClassNames(className)._1))) delegate.findClassFile(className) else None
  private[nsc] def hasPackage(pkg: PackageName) = ok(pkg) && delegate.hasPackage(pkg)
  private[nsc] def list(inPackage: PackageName) = if (ok(inPackage)) delegate.list(inPackage) else ClassPathEntries(Nil, Nil)
  private[nsc] def packages(inPackage: PackageName) = if (ok(inPackage)) delegate.packages(inPackage) else Nil
}

/**
  * Implementation `ClassPath` based on the JDK 9 encapsulated runtime modules (JEP-220)
  *
  * https://bugs.openjdk.java.net/browse/JDK-8066492 is the most up to date reference
  * for the structure of the jrt:// filesystem.
  *
  * The implementation assumes that no classes exist in the empty package.
  */
final class JrtClassPath(fs: FileSystem, closeFS: Boolean) extends ClassPath with NoSourcePaths with Closeable {
  type F = Path
  private val dir: Path = fs.getPath("/packages")

  // e.g. "java.lang" -> Seq("/modules/java.base")
  private val packageToModuleBases: Map[String, Seq[Path]] = {
    val ps = Files.newDirectoryStream(dir).iterator.asScala
    def lookup(pack: Path): Seq[Path] = {
      Files.list(pack).iterator.asScala.map(l => if (Files.isSymbolicLink(l)) Files.readSymbolicLink(l) else l).toList
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
        Files.list(x.resolve(inPackage.dirPathTrailingSlash)).iterator.asScala.filter(_.getFileName.toString.endsWith(".class"))).map(x =>
        ClassFileEntryImpl(new PlainNioFile(x))).toVector
    }
  }

  override private[nsc] def list(inPackage: PackageName): ClassPathEntries =
    if (inPackage.isRoot) ClassPathEntries(packages(inPackage), Nil)
    else ClassPathEntries(packages(inPackage), classes(inPackage))

  def asURLs: Seq[URL] = Seq(new URI("jrt:/").toURL)
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

  def close(): Unit =
    if (closeFS) fs.close()
}

/**
  * Implementation `ClassPath` based on the \$JAVA_HOME/lib/ct.sym backing https://openjdk.java.net/jeps/247
  */
final class CtSymClassPath(ctSym: java.nio.file.Path, release: Int) extends ClassPath with NoSourcePaths with Closeable {
  import java.nio.file.Path, java.nio.file._

  private val fileSystem: FileSystem = FileSystems.newFileSystem(ctSym, null: ClassLoader)
  private val root: Path = fileSystem.getRootDirectories.iterator.next
  private val roots = Files.newDirectoryStream(root).iterator.asScala.toList

  // https://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/011737.html
  private def codeFor(major: Int): String = if (major < 10) major.toString else ('A' + (major - 10)).toChar.toString

  private val releaseCode: String = codeFor(release)
  private def fileNameMatchesRelease(fileName: String) = !fileName.contains("-") && fileName.contains(releaseCode) // exclude `9-modules`
  private val rootsForRelease: List[Path] = roots.filter(root => fileNameMatchesRelease(root.getFileName.toString))

  // e.g. "java.lang" -> Seq(/876/java/lang, /87/java/lang, /8/java/lang))
  private val packageIndex: scala.collection.Map[String, scala.collection.Seq[Path]] = {
    val index = collection.mutable.HashMap[String, collection.mutable.ListBuffer[Path]]()
    val isJava12OrHigher = isJavaAtLeast("12")
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
        Files.list(p).iterator.asScala.filter(_.getFileName.toString.endsWith(".sig")))
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
  protected def isMatchingFile(f: File, siblingExists: String => Boolean): Boolean =
    f.isClass && !(f.getName.endsWith(".class") && siblingExists(classNameToTasty(f.getName)))

  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = files(inPackage)
}

case class DirectorySourcePath(dir: File) extends JFileDirectoryLookup[SourceFileEntryImpl] with NoClassPaths {
  def asSourcePathString: String = asClassPathString

  protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  protected def isMatchingFile(f: File, siblingExists: String => Boolean): Boolean = endsScalaOrJava(f.getName)

  override def findClass(className: String): Option[ClassRepresentation] = findSourceFile(className) map SourceFileEntryImpl

  private def findSourceFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className)
    val sourceFile = Iterator("scala", "java")
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
