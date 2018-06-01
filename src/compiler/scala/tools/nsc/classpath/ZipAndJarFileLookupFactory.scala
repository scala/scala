/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.{File, IOException}
import java.net.URL
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.ZipException

import scala.annotation.tailrec
import scala.reflect.io.{AbstractFile, FileZipArchive, ManifestResources}
import scala.tools.nsc.util.{ClassPath, ClassRepresentation}
import scala.tools.nsc.Settings
import FileUtils._
import scala.collection.mutable.ArrayBuffer

/**
 * A trait providing an optional cache for classpath entries obtained from zip and jar files.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
sealed trait ZipAndJarFileLookupFactory {
  private val cache = new FileBasedCache[ClassPath]

  def create(zipFile: AbstractFile, settings: Settings): ClassPath = {
    if (settings.YdisableFlatCpCaching || zipFile.file == null) createForZipFile(zipFile, settings.releaseValue)
    else createUsingCache(zipFile, settings)
  }

  protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath

  private def createUsingCache(zipFile: AbstractFile, settings: Settings): ClassPath = {
    cache.getOrCreate(List(zipFile.file.toPath), () => createForZipFile(zipFile, settings.releaseValue))
  }
}

/**
 * Manages creation of classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveClassPath(zipFile: File, override val release: Option[String])
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(pkg, simpleClassName + ".class").map(_.file)
    }
    // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
    override def findClass(className: String): Option[ClassRepresentation] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(pkg, simpleClassName + ".class")
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): ClassFileEntryImpl = ClassFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
  }

  /**
   * This type of classpath is closely related to the support for JSR-223.
   * Its usage can be observed e.g. when running:
   * jrunscript -classpath scala-compiler.jar;scala-reflect.jar;scala-library.jar -l scala
   * with a particularly prepared scala-library.jar. It should have all classes listed in the manifest like e.g. this entry:
   * Name: scala/Function2$mcFJD$sp.class
   */
  private case class ManifestResourcesClassPath(file: ManifestResources) extends ClassPath with NoSourcePaths {
    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override def asClassPathStrings: Seq[String] = Seq(file.path)

    override def asURLs: Seq[URL] = file.toURLs()

    import ManifestResourcesClassPath.PackageFileInfo
    import ManifestResourcesClassPath.PackageInfo

    /**
     * A cache mapping package name to abstract file for package directory and subpackages of given package.
     *
     * ManifestResources can iterate through the collections of entries from e.g. remote jar file.
     * We can't just specify the path to the concrete directory etc. so we can't just 'jump' into
     * given package, when it's needed. On the other hand we can iterate over entries to get
     * AbstractFiles, iterate over entries of these files etc.
     *
     * Instead of traversing a tree of AbstractFiles once and caching all entries or traversing each time,
     * when we need subpackages of a given package or its classes, we traverse once and cache only packages.
     * Classes for given package can be then easily loaded when they are needed.
     */
    private lazy val cachedPackages: collection.mutable.HashMap[String, PackageFileInfo] = {
      val packages = collection.mutable.HashMap[String, PackageFileInfo]()

      def getSubpackages(dir: AbstractFile): List[AbstractFile] =
        (for (file <- dir if file.isPackage) yield file)(collection.breakOut)

      @tailrec
      def traverse(packagePrefix: String,
                   filesForPrefix: List[AbstractFile],
                   subpackagesQueue: collection.mutable.Queue[PackageInfo]): Unit = filesForPrefix match {
        case pkgFile :: remainingFiles =>
          val subpackages = getSubpackages(pkgFile)
          val fullPkgName = packagePrefix + pkgFile.name
          packages.put(fullPkgName, PackageFileInfo(pkgFile, subpackages))
          val newPackagePrefix = fullPkgName + "."
          subpackagesQueue.enqueue(PackageInfo(newPackagePrefix, subpackages))
          traverse(packagePrefix, remainingFiles, subpackagesQueue)
        case Nil if subpackagesQueue.nonEmpty =>
          val PackageInfo(packagePrefix, filesForPrefix) = subpackagesQueue.dequeue()
          traverse(packagePrefix, filesForPrefix, subpackagesQueue)
        case _ =>
      }

      val subpackages = getSubpackages(file)
      packages.put(ClassPath.RootPackage, PackageFileInfo(file, subpackages))
      traverse(ClassPath.RootPackage, subpackages, collection.mutable.Queue())
      packages
    }

    override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = cachedPackages.get(inPackage) match {
      case None => Seq.empty
      case Some(PackageFileInfo(_, subpackages)) =>
        val prefix = PackageNameUtils.packagePrefix(inPackage)
        subpackages.map(packageFile => PackageEntryImpl(prefix + packageFile.name))
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = cachedPackages.get(inPackage) match {
      case None => Seq.empty
      case Some(PackageFileInfo(pkg, _)) =>
        (for (file <- pkg if file.isClass) yield ClassFileEntryImpl(file))(collection.breakOut)
    }


    override private[nsc] def hasPackage(pkg: String) = cachedPackages.contains(pkg)
    override private[nsc] def list(inPackage: String): ClassPathEntries = ClassPathEntries(packages(inPackage), classes(inPackage))
  }

  private object ManifestResourcesClassPath {
    case class PackageFileInfo(packageFile: AbstractFile, subpackages: Seq[AbstractFile])
    case class PackageInfo(packageName: String, subpackages: List[AbstractFile])
  }

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath =
    if (zipFile.file == null) createWithoutUnderlyingFile(zipFile)
    else ZipArchiveClassPath(zipFile.file, release)

  private def createWithoutUnderlyingFile(zipFile: AbstractFile) = zipFile match {
    case manifestRes: ManifestResources =>
      ManifestResourcesClassPath(manifestRes)
    case _ =>
      val errorMsg = s"Abstract files which don't have an underlying file and are not ManifestResources are not supported. There was $zipFile"
      throw new IllegalArgumentException(errorMsg)
  }
}

/**
 * Manages creation of classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory {
  private case class ZipArchiveSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {
    def release: Option[String] = None

    override def asSourcePathString: String = asClassPathString

    override private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath = ZipArchiveSourcePath(zipFile.file)
}

final class FileBasedCache[T] {
  import java.nio.file.Path
  private case class Stamp(size: Long, signatures: ArrayBuffer[Long], fileKey: Object)
  private val cache = collection.mutable.Map.empty[Seq[Path], (Seq[Stamp], T)]

  /**
    * Read the CRCs of every zip entry if all of them succeed to be read.
    *
    * This method uses `java.util.jar` instead of `java.util.zip` utilities
    * because the method `getCrc` in [[java.util.zip.ZipEntry]] returns 0 or
    * -1 values.
    *
    * @param jarFile A jar file (which follows the zip specification format).
    * @return A list of checksums for every zip entry.
    */
  def readJarCrcs(jarFile: Path): Option[ArrayBuffer[Long]] = {
    try {
      val jf = new java.util.jar.JarFile(jarFile.toFile)
      try {
        val entries = jf.entries()
        val buffer = new ArrayBuffer[Long](32)
        while (entries.hasMoreElements) buffer.+=(entries.nextElement().getCrc)
        // It is very likely that a jar with no CRCs entries is ill-generated, assume so
        if (buffer.isEmpty || buffer.forall(_ == -1L)) None else Some(buffer)
      } finally jf.close()
    } catch {
      case _: IOException => None
    }
  }

  /**
    * Get a cached value or create it afresh if the value is missing or
    * any of the given paths have been "modified".
    *
    * This implementation considers that a file has been modified if:
    *   1. The file has changed its total size.
    *   2. The CRCs present in the jar file are not the same (in the same order).
    *   3. The file key (uniquely identifying a file) is different.
    *
    * Note that if there's a IO error while reading the CRCs in the cached paths,
    * this implementation fallbacks to the use of the last modified time. This
    * also happens when jars have no CRCs -- it is extremely unlikely that a
    * classpath entry has no CRC, so if that happens we assume the jar is
    * either corrupt or ill-generated, and we use a safer change detection strategy.
    *
    * @param paths The classpath entries to check for modification.
    * @param create The closure to produce elements of type [[T]].
    * @return A result of type [[T]] that may be either recently generated or cached.
    */
  def getOrCreate(paths: Seq[Path], create: () => T): T = cache.synchronized {
    val stamps = paths.map { path =>
      val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
      val currentSize = attrs.size()
      val signatures = readJarCrcs(path).getOrElse(ArrayBuffer(attrs.lastModifiedTime().toMillis))
      val fileKey = attrs.fileKey() // only null on some platforms, but that's okay
      Stamp(currentSize, signatures, fileKey)
    }

    cache.get(paths) match {
      case Some((cachedStamps, cached)) if cachedStamps == stamps => cached
      case _ =>
        val value = create()
        cache.put(paths, (stamps, value))
        value
    }
  }

  def clear(): Unit = cache.synchronized {
    // TODO support closing
    // cache.valuesIterator.foreach(_.close())
    cache.clear()
  }
}
