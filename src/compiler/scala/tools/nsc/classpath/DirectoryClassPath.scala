/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import java.net.URL
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystems, FileVisitOption, Files}
import java.util

import scala.reflect.io.{AbstractFile, PlainFile, PlainNioFile}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}
import FileUtils._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.JDK9Reflectors
import scala.tools.nsc.classpath.PackageNameUtils.{packageContains, separatePkgAndClassNames}

/**
 * A trait allowing to look for classpath entries in directories. It provides common logic for
 * classes handling class and source files.
 * It makes use of the fact that in the case of nested directories it's easy to find a file
 * when we have a name of a package.
 * It abstracts over the file representation to work with both JFile and AbstractFile.
 */
trait DirectoryLookup[FileEntryType <: ClassRepresentation] extends ClassPath {
  // Within this file, we sort by file name for stable order of directory entries in package scope.
  // This gives stable results ordering of base type sequences for unrelated classes
  // with the same base type depth.
  //
  // Notably, this will stably infer`Product with Serializable`
  // as the type of `case class C(); case class D(); List(C(), D()).head`, rather than the opposite order.
  // On Mac, the HFS performs this sorting transparently, but on Linux the order is unspecified.
  //
  // Note this behaviour can be enabled in javac with `javac -XDsortfiles`, but that's only
  // intended to improve determinism of the compiler for compiler hackers.


  type F

  val dir: F

  protected def emptyFiles: Array[F] // avoids reifying ClassTag[F]
  protected def getSubDir(dirName: String): Option[F]
  protected def listChildren(dir: F)(f: (F, BasicFileAttributes) => Boolean): (Array[F], Array[F])
  protected def getName(f: F): String
  protected def toAbstractFile(f: F): AbstractFile
  protected def isPackage(f: F, attrs: BasicFileAttributes): Boolean

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isMatchingFile(f: F, attrs: BasicFileAttributes): Boolean

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
    dirForPackage match {
      case None => Nil
      case Some(directory) =>
        val prefix = PackageNameUtils.packagePrefix(inPackage)
        val (listing, files) = listChildren(directory) { (childFile, attrs) => isPackage(childFile, attrs)}
        assert(files.isEmpty, files.toList)
        val array = listing.map(f => PackageEntryImpl(prefix + getName(f)))
        collection.immutable.ArraySeq.unsafeWrapArray(array)
    }
  }

  protected def files(inPackage: String): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    dirForPackage match {
      case None => Nil
      case Some(directory) =>
        val (dirs, listing) = listChildren(directory) { (childFile, attrs) => isMatchingFile(childFile, attrs) }
        assert(dirs.isEmpty, dirs.toList)
        listing.iterator.map(f => createFileEntry(toAbstractFile(f))).toVector
    }
  }

  private[nsc] def list(inPackage: String): ClassPathEntries = {
    val dirForPackage = getDirectory(inPackage)
    dirForPackage match {
      case None => ClassPathEntries.empty
      case Some(directory) =>
        val packagePrefix = PackageNameUtils.packagePrefix(inPackage)
        val (dirs, files) = listChildren(directory) {
          (childFile, attrs) => isPackage(childFile, attrs) || isMatchingFile(childFile, attrs)
        }
        ClassPathEntries(
          dirs.map(f => PackageEntryImpl(packagePrefix + getName(f))),
          ArrayBuffer.from(files.view.map(f => createFileEntry(toAbstractFile(f))))
        )
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

  override protected def listChildren(dir: File)(f: (File, BasicFileAttributes) => Boolean): (Array[File], Array[File]) = {
    import java.nio.file._
    val files = new collection.mutable.ArrayBuilder.ofRef[File]()
    val dirs = new collection.mutable.ArrayBuilder.ofRef[File]()
    val visitor = new SimpleFileVisitor[Path]() {
      override def visitFile(childFile: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
        if (dir != childFile && f(childFile.toFile, attrs)) {
          if (attrs.isDirectory) dirs += childFile.toFile
          else files += childFile.toFile
        }
        FileVisitResult.CONTINUE
      }
    }
    // OPT: Prefer walkFileTree to File#listFiles, especially on Windows: https://bugs.openjdk.java.net/browse/JDK-8008469
    Files.walkFileTree(dir.toPath, util.EnumSet.of(FileVisitOption.FOLLOW_LINKS), 1, visitor)
    val filesListing = files.result()
    val dirsListing = dirs.result()
    util.Arrays.sort(filesListing, (o1: File, o2: File) => o1.getName.compareTo(o2.getName))
    util.Arrays.sort(dirsListing, (o1: File, o2: File) => o1.getName.compareTo(o2.getName))
    (dirsListing, filesListing)
  }

  protected def getName(f: File): String = f.getName
  protected def toAbstractFile(f: File): AbstractFile = new PlainFile(new scala.reflect.io.File(f))
  protected def isPackage(f: File, attrs: BasicFileAttributes): Boolean = attrs.isDirectory && mayBeValidPackage(f.getName)

  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  def asURLs: Seq[URL] = Seq(dir.toURI.toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.getPath)
}

object JrtClassPath {
  import java.nio.file._, java.net.URI
  def apply(release: Option[String]): Option[ClassPath] = {
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
            else Some(new CtSymClassPath(ctSym, v.toInt))
          } catch {
            case _: Throwable => None
          }
        case _ =>
          try {
            val fs = FileSystems.getFileSystem(URI.create("jrt:/"))
            Some(new JrtClassPath(fs))
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
    val ps = Files.newDirectoryStream(dir).iterator.asScala
    def lookup(pack: Path): Seq[Path] = {
      Files.list(pack).iterator.asScala.map(l => if (Files.isSymbolicLink(l)) Files.readSymbolicLink(l) else l).toList
    }
    ps.map(p => (p.toString.stripPrefix("/packages/"), lookup(p))).toMap
  }

  /** Empty string represents root package */
  override private[nsc] def hasPackage(pkg: String) = packageToModuleBases.contains(pkg)
  override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    packageToModuleBases.keysIterator.filter(pack => packageContains(inPackage, pack)).map(PackageEntryImpl(_)).toVector
  }
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = {
    if (inPackage == "") Nil
    else {
      packageToModuleBases.getOrElse(inPackage, Nil).flatMap(x =>
        Files.list(x.resolve(inPackage.replace('.', '/'))).iterator.asScala.filter(_.getFileName.toString.endsWith(".class"))).map(x =>
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
      val (inPackage, _) = separatePkgAndClassNames(className)
      packageToModuleBases.getOrElse(inPackage, Nil).iterator.flatMap { x =>
        val file = x.resolve(className.replace('.', '/') + ".class")
        if (Files.exists(file)) new scala.reflect.io.PlainNioFile(file) :: Nil else Nil
      }.take(1).toList.headOption
    }
  }
}

/**
  * Implementation `ClassPath` based on the $JAVA_HOME/lib/ct.sym backing http://openjdk.java.net/jeps/247
  */
final class CtSymClassPath(ctSym: java.nio.file.Path, release: Int) extends ClassPath with NoSourcePaths {
  import java.nio.file.Path, java.nio.file._

  private val fileSystem: FileSystem = FileSystems.newFileSystem(ctSym, null)
  private val root: Path = fileSystem.getRootDirectories.iterator.next
  private val roots = Files.newDirectoryStream(root).iterator.asScala.toList

  // http://mail.openjdk.java.net/pipermail/compiler-dev/2018-March/011737.html
  private def codeFor(major: Int): String = if (major < 10) major.toString else ('A' + (major - 10)).toChar.toString

  private val releaseCode: String = codeFor(release)
  private def fileNameMatchesRelease(fileName: String) = !fileName.contains("-") && fileName.contains(releaseCode) // exclude `9-modules`
  private val rootsForRelease: List[Path] = roots.filter(root => fileNameMatchesRelease(root.getFileName.toString))

  // e.g. "java.lang" -> Seq(/876/java/lang, /87/java/lang, /8/java/lang))
  private val packageIndex: scala.collection.Map[String, scala.collection.Seq[Path]] = {
    val index = collection.mutable.AnyRefMap[String, collection.mutable.ListBuffer[Path]]()
    rootsForRelease.foreach(root => Files.walk(root).iterator.asScala.filter(Files.isDirectory(_)).foreach { p =>
      if (p.getNameCount > 1) {
        val packageDotted = p.subpath(1, p.getNameCount).toString.replace('/', '.')
        index.getOrElseUpdate(packageDotted, new collection.mutable.ListBuffer) += p
      }
    })
    index
  }

  /** Empty string represents root package */
  override private[nsc] def hasPackage(pkg: String) = packageIndex.contains(pkg)
  override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    packageIndex.keysIterator.filter(pack => packageContains(inPackage, pack)).map(PackageEntryImpl(_)).toVector
  }
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = {
    if (inPackage == "") Nil
    else {
      val sigFiles = packageIndex.getOrElse(inPackage, Nil).iterator.flatMap(p =>
        Files.list(p).iterator.asScala.filter(_.getFileName.toString.endsWith(".sig")))
      sigFiles.map(f => ClassFileEntryImpl(new PlainNioFile(f))).toVector
    }
  }

  override private[nsc] def list(inPackage: String): ClassPathEntries =
    if (inPackage == "") ClassPathEntries(packages(inPackage), Nil)
    else ClassPathEntries(packages(inPackage), classes(inPackage))

  def asURLs: Seq[URL] = Nil
  def asClassPathStrings: Seq[String] = Nil

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
  protected def isMatchingFile(f: File, attrs: BasicFileAttributes): Boolean = attrs.isRegularFile && endsClass(f.getName)

  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)
}

case class DirectorySourcePath(dir: File) extends JFileDirectoryLookup[SourceFileEntryImpl] with NoClassPaths {
  def asSourcePathString: String = asClassPathString

  protected def createFileEntry(file: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(file)
  protected def isMatchingFile(f: File, attrs: BasicFileAttributes): Boolean = endsScalaOrJava(f.getName)

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
