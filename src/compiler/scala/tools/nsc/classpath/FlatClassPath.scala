/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.{ ClassFileLookup, ClassPath, ClassRepresentation }

/**
 * A base trait for the particular flat classpath representation implementations.
 *
 * We call this variant of a classpath representation flat because it's possible to
 * query the whole classpath using just single instance extending this trait.
 *
 * This is an alternative design compared to scala.tools.nsc.util.ClassPath
 */
trait FlatClassPath extends ClassFileLookup[AbstractFile] {
  /** Empty string represents root package */
  private[nsc] def packages(inPackage: String): Seq[PackageEntry]
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry]
  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry]

  /** Allows to get entries for packages and classes merged with sources possibly in one pass. */
  private[nsc] def list(inPackage: String): FlatClassPathEntries

  // A default implementation which should be overridden, if we can create the more efficient
  // solution for a given type of FlatClassPath
  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)

    val foundClassFromClassFiles = classes(pkg)
      .find(_.name == simpleClassName)

    def findClassInSources = sources(pkg)
      .find(_.name == simpleClassName)

    foundClassFromClassFiles orElse findClassInSources
  }

  override def asClassPathString: String = ClassPath.join(asClassPathStrings: _*)
  def asClassPathStrings: Seq[String]
}

object FlatClassPath {
  val RootPackage = ""
}

case class FlatClassPathEntries(packages: Seq[PackageEntry], classesAndSources: Seq[ClassRepClassPathEntry])

object FlatClassPathEntries {
  import scala.language.implicitConversions
  // to have working unzip method
  implicit def entry2Tuple(entry: FlatClassPathEntries) = (entry.packages, entry.classesAndSources)
}

sealed trait ClassRepClassPathEntry extends ClassRepresentation[AbstractFile]

trait ClassFileEntry extends ClassRepClassPathEntry {
  def file: AbstractFile
}

trait SourceFileEntry extends ClassRepClassPathEntry {
  def file: AbstractFile
}

trait PackageEntry {
  def name: String
}

private[nsc] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry {
  override def name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

private[nsc] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  override def name = FileUtils.stripSourceExtension(file.name)

  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

private[nsc] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepClassPathEntry {
  override def name = FileUtils.stripClassExtension(classFile.name)

  override def binary: Option[AbstractFile] = Some(classFile)
  override def source: Option[AbstractFile] = Some(srcFile)
}

private[nsc] case class PackageEntryImpl(name: String) extends PackageEntry

private[nsc] trait NoSourcePaths {
  def asSourcePathString: String = ""
  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty
}

private[nsc] trait NoClassPaths {
  def findClassFile(className: String): Option[AbstractFile] = None
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] = Seq.empty
}
