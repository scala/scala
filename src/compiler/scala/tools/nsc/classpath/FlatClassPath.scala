/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, ClassFileLookup, ClassRepresentation}
import scala.reflect.io.File._

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
  def packages(inPackage: String): Seq[PackageEntry]
  def classes(inPackage: String): Seq[FileEntry]
  def list(inPackage: String): (Seq[PackageEntry], Seq[FileEntry])

  // TODO temporary solution - it's very inefficient and we loose benefits which we have from having specialised classes
  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    classes(pkg).find(_.name == simpleClassName)
  }

  override def asClassPathString: String = ClassPath.join(asClassPathStrings: _*)
  def asClassPathStrings: Seq[String]
}

object FlatClassPath {
  val RootPackage = ""
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

// TODO don't we have problems with some duplicated dirs in cp and sp when fir will be set in both places? Check that
trait ClassFileEntry extends FileEntry {
  override def binary: Option[AbstractFile] = Some(file) // TODO temporary solution due to compatibility with sbt's CompilerInterface which requires such methods
  override def source: Option[AbstractFile] = None
}

trait SourceFileEntry extends FileEntry {
  override def binary: Option[AbstractFile] = None
  override def source: Option[AbstractFile] = Some(file)
}

case class SourceFileEntryImpl(file: AbstractFile) extends SourceFileEntry

trait PackageEntry extends ClassPathEntry

case class PackageEntryImpl(name: String) extends PackageEntry

trait NoSourcePaths {
  def asSourcePathString: String = ""
}
