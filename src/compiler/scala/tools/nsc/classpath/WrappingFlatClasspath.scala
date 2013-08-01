package scala.tools.nsc.classpath

import scala.tools.nsc.util.ClassPath
import scala.reflect.io.AbstractFile

class WrappingFlatClasspath(wrappedClasspath: ClassPath[AbstractFile]) extends FlatClasspath {
  /** Empty string represents root package */
  def packages(inPackage: String): Seq[String] = {
    if (inPackage == "") {
      wrappedClasspath.packages.map(_.name)
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      wrappedPackage.packages.map(pkg => inPackage + "." + pkg.name)
    }
  }
  def classes(inPackage: String): Seq[String] = {
    if (inPackage == "") {
      wrappedClasspath.classes.map(_.name)
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      val classes = wrappedPackage.classes.map(classRep => inPackage + "." + classRep.name)
      classes
    }
  }
  def loadClassfile(classfile: String): Array[Byte] = {
    val binaryFile = wrappedClasspath.findClass(classfile).get.binary.get
    binaryFile.toByteArray
  }

  private def selectWrappedPackage(pkg: String): ClassPath[AbstractFile] = {
    val packageNames: Seq[String] = pkg.split('.')
    // package corresponding to inPackage coming from wrappedClasspath
    val wrappedPackage = packageNames.foldLeft(wrappedClasspath) {
      case (wrappedPackage, pkgName) => wrappedPackage.packages.find(pkg => pkg.name == pkgName).get
    }
    wrappedPackage
  }
}
