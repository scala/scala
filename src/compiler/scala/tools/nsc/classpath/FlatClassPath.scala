/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, ClassFileLookup, ClassRepresentation}

/**
 * An interface for a flat classpath.
 *
 * We call this variant of a classpath representation flat because you can
 * query the whole classpath using just single instance implementing this interface.
 *
 * This is an alternative design compared to scala.tools.nsc.util.ClassPath
 */
trait FlatClassPath extends ClassFileLookup[AbstractFile] {
  /** Empty string represents root package */
  // TODO we probably don't need packages on this level anymore as we have list operation
  def packages(inPackage: String): Seq[PackageEntry]
  // TODO and also classes and sources could be then protected
  def classes(inPackage: String): Seq[ClassFileEntry]
  def sources(inPackage: String): Seq[SourceFileEntry]

  def list(inPackage: String): FlatClassPathEntries

  // TODO it would be nice to have some internal check for name - not after returning all classes/sources in pkg
  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)

    val foundClassFile = classes(pkg)
      .find(_.name == simpleClassName)

    def findSourceFile = sources(pkg)
      .find(_.name == simpleClassName)

    foundClassFile orElse findSourceFile
  }

  override def asClassPathString: String = ClassPath.join(asClassPathStrings: _*)
  def asClassPathStrings: Seq[String]

}

object FlatClassPath {
  val RootPackage = ""
}

case class FlatClassPathEntries(packages: Seq[PackageEntry], classesAndSources: Seq[FileEntry])

object FlatClassPathEntries {
  import scala.language.implicitConversions
  // to have working unzip method
  implicit def entry2Tuple(entry: FlatClassPathEntries) = (entry.packages, entry.classesAndSources)
}

trait NoSourcePaths {
  def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty
  def asSourcePathString: String = ""
}

trait NoClassPaths {
  def classes(inPackage: String): Seq[ClassFileEntry] = Seq.empty
  def findClassFile(className: String): Option[AbstractFile] = None
}

sealed trait ClassPathEntry {
  def name: String
}

trait FileEntry extends ClassPathEntry with ClassRepresentation[AbstractFile] {
  def file: AbstractFile

  override def name = {
    val className = FileUtils.stripClassExtension(file.name)
    className
  }
}

trait ClassFileEntry extends FileEntry {
  override def binary: Option[AbstractFile] = Some(file) // TODO temporary solution due to compatibility with sbt's CompilerInterface which requires such methods
  override def source: Option[AbstractFile] = None
}

trait SourceFileEntry extends FileEntry {
  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry

case class ClassAndSourceFilesEntry(file: AbstractFile, src: AbstractFile) extends FileEntry {
  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = Some(src)
}

trait PackageEntry extends ClassPathEntry

case class PackageEntryImpl(name: String) extends PackageEntry
