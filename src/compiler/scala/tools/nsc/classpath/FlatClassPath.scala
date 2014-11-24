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
  // it can be discussed later
  def packages(inPackage: String): Seq[PackageEntry]
  def classes(inPackage: String): Seq[ClassFileEntry]
  def sources(inPackage: String): Seq[SourceFileEntry]

  def list(inPackage: String): FlatClassPathEntries

  /**
   * A default implementation which should be overriden, if we can create more efficient
   * solution for given type of FlatClassPath
   */
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

trait ClassRepClassPathEntry extends ClassPathEntry with ClassRepresentation[AbstractFile]

trait ClassFileEntry extends ClassRepClassPathEntry {
  def file: AbstractFile

  override def name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry

trait SourceFileEntry extends ClassRepClassPathEntry {
  def file: AbstractFile

  override def name = FileUtils.stripSourceExtension(file.name) // class name

  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry

case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepClassPathEntry {
  override def name = FileUtils.stripClassExtension(classFile.name) // class name

  override def binary: Option[AbstractFile] = Some(classFile)
  override def source: Option[AbstractFile] = Some(srcFile)
}

trait PackageEntry extends ClassPathEntry

case class PackageEntryImpl(name: String) extends PackageEntry
