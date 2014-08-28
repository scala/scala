/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.io.File
import scala.reflect.io.{AbstractFile, FileZipArchive}
import scala.tools.nsc.Settings
import FileUtils._

trait ZipAndJarFileLookupFactory {

  private val cache = collection.mutable.Map.empty[AbstractFile, FlatClassPath]

  def create(zipFile: AbstractFile, settings: Settings): FlatClassPath = {
    if (settings.YflatCpCaching) createUsingCache(zipFile, settings)
    else createForZipFile(zipFile)
  }

  protected def createForZipFile(zipFile: AbstractFile): FlatClassPath

  private def createUsingCache(zipFile: AbstractFile, settings: Settings): FlatClassPath = {
    def newArchive = {
      if (settings.verbose || settings.Ylogcp)
      // TODO maybe use some logger instead of println?
        println(s"Missed cache for $zipFile")
      createForZipFile(zipFile)
    }
    cache.getOrElseUpdate(zipFile, newArchive)
  }
}

object ZipAndJarFlatClassPathFactory extends ZipAndJarFileLookupFactory {

  import ZipArchiveFlatClassPath.ClassFileEntryImpl
  case class ZipArchiveFlatClassPath private[ZipAndJarFlatClassPathFactory](zipFile: File)
    extends ZipArchiveFileLookup[ClassFileEntryImpl]
    with NoSourcePaths {

    override def findClassFile(className: String): Option[AbstractFile] = {
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(pkg).find(_.name == simpleClassName).map(_.file)
    }

    override def classes(inPackage: String): Seq[ClassFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): ClassFileEntryImpl = ClassFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isClass
  }

  object ZipArchiveFlatClassPath {

    private[classpath] case class ClassFileEntryImpl(entry: FileZipArchive#Entry) extends ClassFileEntry {
      override def file: AbstractFile = entry
    }
  }

  override protected def createForZipFile(zipFile: AbstractFile): FlatClassPath =
  // FIXME e.g. ManifestResources - have to be handled in other way - unfortunately our implementation
  // for abstract file didn't work here because it uses certain methods which are unsupported be given
  // implementation of abstract file
    if (zipFile.file == null) ???
    else ZipArchiveFlatClassPath(zipFile.file)
}

object ZipAndJarFlatSourcePathFactory extends ZipAndJarFileLookupFactory {

  case class ZipArchiveFlatSourcePath private[ZipAndJarFlatSourcePathFactory](zipFile: File)
    extends ZipArchiveFileLookup[SourceFileEntryImpl]
    with NoClassPaths {

    override def sources(inPackage: String): Seq[SourceFileEntry] = files(inPackage)

    override def asSourcePathString: String = asClassPathString

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntryImpl = SourceFileEntryImpl(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource
  }

  override protected def createForZipFile(zipFile: AbstractFile): FlatClassPath = ZipArchiveFlatSourcePath(zipFile.file)
}
