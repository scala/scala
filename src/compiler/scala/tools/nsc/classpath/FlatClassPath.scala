/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassFileLookup
import scala.tools.nsc.util.ClassRepresentation

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
  def classes(inPackage: String): Seq[ClassFileEntry]
  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry])
  //def sourcepath: Seq[SourceFileEntry?] // FIXME implement sourcepath

  // FIXME implement this
  override def findClass(name: String): Option[ClassRepresentation[AbstractFile]] = ???
}

object FlatClassPath {
  val RootPackage = ""
}

sealed trait ClasspathEntry {
  def name: String
}

trait ClassFileEntry extends ClasspathEntry {
  def file: AbstractFile
}

trait PackageEntry extends ClasspathEntry

case class PackageEntryImpl(name: String) extends PackageEntry
