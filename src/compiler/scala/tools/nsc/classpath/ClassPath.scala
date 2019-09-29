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

import scala.collection.mutable
import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassRepresentation
case class ClassPathEntries private (packages: Seq[PackageEntry], classesAndSources: Seq[ClassRepresentation]) {
  def isEmpty = packages.isEmpty && classesAndSources.isEmpty

  lazy val classesAndSourcesByName: Map[String, ClassRepresentation] = classesAndSources.map(e => e.name -> e)(collection.breakOut)
  lazy val sources: Seq[SourceFileEntry] = collectCompressed { case s: SourceFileEntry => s }
  lazy val classes: Seq[ClassFileEntry] = collectCompressed { case c: ClassFileEntry => c }

  private def collectCompressed[T <: ClassRepresentation] (fn: PartialFunction[ClassRepresentation, T]): Seq[T] = {
    val raw = classesAndSources.collect {fn}
    if (raw.isEmpty) Nil
    else if (raw.size == classesAndSources.size) classesAndSources.asInstanceOf[Seq[T]] //they are ==
    else raw
  }

}

object ClassPathEntries {
  import scala.language.implicitConversions
  // to have working unzip method
  implicit def entry2Tuple(entry: ClassPathEntries): (Seq[PackageEntry], Seq[ClassRepresentation]) = (entry.packages, entry.classesAndSources)
  val empty = new ClassPathEntries(Seq.empty, Seq.empty)
  def apply(packages: Seq[PackageEntry], classesAndSources: Seq[ClassRepresentation]): ClassPathEntries = {
    if (packages.isEmpty && classesAndSources.isEmpty) empty else {
      def compress[T <: AnyRef: Manifest](seq: Seq[T]): Seq[T] = seq match {
        case Nil => Nil
        case List(one) => List(one)
        case wa : mutable.WrappedArray[T] => wa
        case _ => new mutable.WrappedArray.ofRef(seq.toArray)
      }
      new ClassPathEntries(compress(packages), compress(classesAndSources))
    }
  }
}

trait ClassFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

trait SourceFileEntry extends ClassRepresentation {
  def file: AbstractFile
}

case class PackageName(dottedString: String) {
  def isRoot: Boolean = dottedString.isEmpty
  lazy val dirPathTrailingSlash: String = FileUtils.dirPath(dottedString) + "/"

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
  override val name = FileUtils.stripClassExtension(file.name) // class name

  override def binary: Option[AbstractFile] = Some(file)
  override def source: Option[AbstractFile] = None
}

private[nsc] case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry {
  override val name = FileUtils.stripSourceExtension(file.name)

  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

private[nsc] case class ClassAndSourceFilesEntry(classFile: AbstractFile, srcFile: AbstractFile) extends ClassRepresentation {
  override val name = FileUtils.stripClassExtension(classFile.name)

  override def binary: Option[AbstractFile] = Some(classFile)
  override def source: Option[AbstractFile] = Some(srcFile)
}

private[nsc] case class PackageEntryImpl(name: String) extends PackageEntry

private[nsc] trait NoSourcePaths {
  final def asSourcePathString: String = ""
  final private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] = Seq.empty
}

private[nsc] trait NoClassPaths {
  final def findClassFile(className: String): Option[AbstractFile] = None
  private[nsc] final def classes(inPackage: PackageName): Seq[ClassFileEntry] = Seq.empty
}
