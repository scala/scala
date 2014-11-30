/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import scala.reflect.io.{ AbstractFile, FileZipArchive }
import scala.tools.nsc.Settings
import FileUtils._

/**
 * A trait providing a cache for classpath entries obtained from zip and jar files.
 * It's possible to create such a cache assuming that entries in such files won't change (at
 * least will be the same each time we'll load classpath during the lifetime of JVM process)
 * - unlike class and source files in directories, which can be modified and recompiled.
 * It allows us to e.g. reduce significantly memory used by PresentationCompilers in Scala IDE
 * when there are a lot of projects having a lot of common dependencies.
 */
trait ZipAndJarFileLookupFactory {

  private val cache = collection.mutable.Map.empty[AbstractFile, FlatClassPath]

  def create(zipFile: AbstractFile, settings: Settings): FlatClassPath = cache.synchronized {
    def newClassPathInstance = {
      if (settings.verbose || settings.Ylogcp)
        println(s"$zipFile is not yet in the classpath cache")
      createForZipFile(zipFile)
    }
    cache.getOrElseUpdate(zipFile, newClassPathInstance)
  }

  protected def createForZipFile(zipFile: AbstractFile): FlatClassPath
}

/**
 * Manages creation of flat classpath for class files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarFlatClassPathFactory extends ZipAndJarFileLookupFactory {

  private case class ZipArchiveFlatClassPath(zipFile: File)
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): ClassFileEntryImpl = ClassFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
  }

  override protected def createForZipFile(zipFile: AbstractFile): FlatClassPath =
    if (zipFile.file == null) {
      val errorMsg = s"Abstract files which don't have an underlying file are not supported. There was $zipFile"
      throw new IllegalArgumentException(errorMsg)
    } else ZipArchiveFlatClassPath(zipFile.file)
}

/**
 * Manages creation of flat classpath for source files placed in zip and jar files.
 * It should be the only way of creating them as it provides caching.
 */
object ZipAndJarFlatSourcePathFactory extends ZipAndJarFileLookupFactory {

  private case class ZipArchiveFlatSourcePath(zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {

    override def asSourcePathString: String = asClassPathString

    override private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile): FlatClassPath = ZipArchiveFlatSourcePath(zipFile.file)
}
