/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassRepresentation

case class ClassPathEntries(packages: scala.collection.Seq[PackageEntry], classesAndSources: scala.collection.Seq[ClassRepresentation])

object ClassPathEntries {
  import scala.language.implicitConversions
  // to have working unzip method
  implicit def entry2Tuple(entry: ClassPathEntries): (scala.collection.Seq[PackageEntry], scala.collection.Seq[ClassRepresentation]) = (entry.packages, entry.classesAndSources)
  val empty = ClassPathEntries(Seq.empty, Seq.empty)
}

trait ClassFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

trait SourceFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

trait PackageEntry {
  def name: String
}

private[nsc] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry {
  override val name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

private[nsc] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  override val name = FileUtils.stripSourceExtension(file.name)

  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

private[nsc] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile, override val jpmsModuleName: String = "") extends ClassRepresentation {
  override val name = FileUtils.stripClassExtension(classFile.name)

  override def binary: Option[AbstractFile] = Some(classFile)
  override def source: Option[AbstractFile] = Some(srcFile)
}

private[nsc] case class PackageEntryImpl(name: String) extends PackageEntry

private[nsc] trait NoSourcePaths {
  final def asSourcePathString: String = ""
  final private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] = Seq.empty
}

private[nsc] trait NoClassPaths {
  final def findClassFile(className: String): Option[AbstractFile] = None
  private[nsc] final def classes(inPackage: String): Seq[ClassFileEntry] = Seq.empty
}
