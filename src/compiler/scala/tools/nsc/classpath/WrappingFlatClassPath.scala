/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import java.net.URL
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.util.ClassRepresentation
import scala.reflect.io.AbstractFile

// TODO why wrapping old implementation? when do we need that? (right now it's unused)
class WrappingFlatClassPath(wrappedClasspath: ClassPath[AbstractFile]) extends FlatClassPath {

  /** Empty string represents root package */
  override def packages(inPackage: String): Seq[PackageEntry] = {
    if (inPackage == FlatClassPath.RootPackage) {
      wrappedClasspath.packages.map(pkg => new WrappingPackageEntry(pkg.name, pkg))
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      wrappedPackage.packages.map(pkg => new WrappingPackageEntry(s"$inPackage.${pkg.name}", pkg))
    }
  }

  override def classes(inPackage: String): Seq[ClassFileEntry] = {
    if (inPackage == FlatClassPath.RootPackage) {
      wrappedClasspath.classes.map(classRep => new WrappingClassFileEntry(classRep))
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      val classes = wrappedPackage.classes.map(classRep => new WrappingClassFileEntry(classRep))
      classes
    }
  }
  
  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) =
    (packages(inPackage), classes(inPackage))

  def loadClassfile(classfile: String): Array[Byte] = {
    val binaryFile = wrappedClasspath.findClass(classfile).get.binary.get
    binaryFile.toByteArray
  }

  private def selectWrappedPackage(pkg: String): ClassPath[AbstractFile] = {
    val packageNames: Seq[String] = pkg.split('.')
    // package corresponding to inPackage coming from wrappedClasspath
    val wrappedPackage = packageNames.foldLeft(wrappedClasspath) {
      case (wrappedPackage, pkgName) => 
        wrappedPackage.packages.find(pkg => pkg.name == pkgName).get
    }
    wrappedPackage
  }

  override def findClassFile(name: String): Option[AbstractFile] = wrappedClasspath.findClassFile(name)

  // FIXME implement this
  override def asURLs: Seq[URL] = ???

  protected class WrappingPackageEntry(
      val name: String,
      wrappedPackage: ClassPath[AbstractFile]) extends PackageEntry

  protected class WrappingClassFileEntry(wrappedClassFileRep: ClassRepresentation[AbstractFile])
    extends ClassFileEntry {
    assert(wrappedClassFileRep.binary.isDefined)
    override def name = wrappedClassFileRep.name
    override def file: AbstractFile = wrappedClassFileRep.binary.get
  }
}
