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

class WrappingFlatClasspath(wrappedClasspath: ClassPath[AbstractFile]) extends FlatClasspath {

  /** Empty string represents root package */
  override def packages(inPackage: String): Seq[PackageEntry] = {
    if (inPackage == FlatClasspath.RootPackage) {
      wrappedClasspath.packages.map(pkg => new WrappingPackageEntry(pkg.name, pkg))
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      wrappedPackage.packages.map(pkg => new WrappingPackageEntry(s"$inPackage.${pkg.name}", pkg))
    }
  }

  override def classes(inPackage: String): Seq[ClassfileEntry] = {
    if (inPackage == FlatClasspath.RootPackage) {
      wrappedClasspath.classes.map(classRep => new WrappingClassfileEntry(classRep))
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      val classes = wrappedPackage.classes.map(classRep => new WrappingClassfileEntry(classRep))
      classes
    }
  }
  
  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassfileEntry]) = 
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

  protected class WrappingPackageEntry( // TODO what's that?
      val name: String,
      wrappedPackage: ClassPath[AbstractFile]) extends PackageEntry

  protected class WrappingClassfileEntry(wrappedClassFileRep: ClassRepresentation[AbstractFile])
    extends ClassfileEntry {
    assert(wrappedClassFileRep.binary.isDefined)
    override def name = wrappedClassFileRep.name
    override def file: AbstractFile = wrappedClassFileRep.binary.get
  }
}
