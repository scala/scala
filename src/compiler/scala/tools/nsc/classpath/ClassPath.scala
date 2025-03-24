/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.classpath

import scala.annotation.unused
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

case class PackageName(dottedString: String) {
  def isRoot: Boolean = dottedString.isEmpty
  val dirPathTrailingSlash: String = FileUtils.dirPath(dottedString) + "/"

  def entryName(entry: String): String = {
    if (isRoot) entry else {
      val builder = new java.lang.StringBuilder(dottedString.length + 1 + entry.length)
      builder.append(dottedString)
      builder.append('.')
      builder.append(entry)
      builder.toString
    }
  }
}

trait PackageEntry {
  def name: String
}

private[nsc] case class ClassFileEntryImpl(file: AbstractFile) extends ClassFileEntry {
  override final def fileName: String = file.name
  override lazy val name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

private[nsc] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  override final def fileName: String = file.name
  override lazy val name = FileUtils.stripSourceExtension(file.name)

  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

private[nsc] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepresentation {
  override final def fileName: String = classFile.name
  override lazy val name = FileUtils.stripClassExtension(fileName)
  override def binary: Option[AbstractFile] = Some(classFile)
  override def source: Option[AbstractFile] = Some(srcFile)
}

private[nsc] case class PackageEntryImpl(name: String) extends PackageEntry

private[nsc] trait NoSourcePaths {
  final def asSourcePathString: String = ""
  final private[nsc] def sources(@unused inPackage: PackageName): Seq[SourceFileEntry] = Seq.empty
}

private[nsc] trait NoClassPaths {
  final def findClassFile(className: String): Option[AbstractFile] = None
  private[nsc] final def classes(@unused inPackage: PackageName): Seq[ClassFileEntry] = Seq.empty
}
