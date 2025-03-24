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

import scala.tools.nsc.util.ClassRepresentation
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import FileUtils._
import java.net.{URI, URL}

import scala.reflect.internal.util.AbstractFileClassLoader
import scala.tools.nsc.util.ClassPath

case class VirtualDirectoryClassPath(dir: VirtualDirectory) extends ClassPath with DirectoryLookup[ClassFileEntryImpl] with NoSourcePaths {
  type F = AbstractFile

  protected def emptyFiles: Array[AbstractFile] = Array.empty
  protected def getSubDir(packageDirName: String): Option[AbstractFile] =
    Option(AbstractFileClassLoader.lookupPath(dir)(packageDirName.split('/').toIndexedSeq, directory = true))
  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean] = None): Array[F] = filter match {
    case Some(f) => dir.iterator.filter(f).toArray
    case _ => dir.toArray
  }
  protected def hasChild(dir: AbstractFile, name: String): Boolean = dir.lookupName(name, directory = false) != null

  def getName(f: AbstractFile): String = f.name
  def toAbstractFile(f: AbstractFile): AbstractFile = f
  def isPackage(f: AbstractFile): Boolean = f.isPackage

  // mimic the behavior of the old nsc.util.DirectoryClassPath
  def asURLs: Seq[URL] = Seq(new URI("file://_VIRTUAL_/" + dir.name).toURL)
  def asClassPathStrings: Seq[String] = Seq(dir.path)
  override def findClass(className: String): Option[ClassRepresentation] = findClassFile(className) map ClassFileEntryImpl

  def findClassFile(className: String): Option[AbstractFile] = {
    val relativePath = FileUtils.dirPath(className) + ".class"
    Option(AbstractFileClassLoader.lookupPath(dir)(relativePath.split('/').toIndexedSeq, directory = false))
  }

  private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] = files(inPackage)

  protected def createFileEntry(file: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(file)
  protected def isMatchingFile(f: AbstractFile, siblingExists: String => Boolean): Boolean =
    f.isClass && !(f.hasExtension("class") && siblingExists(classNameToTasty(f.name)))
}
