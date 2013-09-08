package scala.tools.nsc.classpath

import scala.tools.nsc.util.ClassPath
import scala.reflect.io.AbstractFile

class WrappingFlatClasspath(wrappedClasspath: ClassPath[AbstractFile]) extends FlatClasspath {
  /** Empty string represents root package */
  def packages(inPackage: String): Seq[PackageEntry] = {
    if (inPackage == FlatClasspath.RootPackage) {
      wrappedClasspath.packages.map(pkg => new WrappingPackageEntry(pkg.name, pkg))
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      wrappedPackage.packages.map(pkg => new WrappingPackageEntry(inPackage + "." + pkg.name, pkg))
    }
  }
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    if (inPackage == FlatClasspath.RootPackage) {
      wrappedClasspath.classes.map(classRep => new WrappingClassfileEntry(classRep))
    } else {
      val wrappedPackage = selectWrappedPackage(inPackage)
      val classes = wrappedPackage.classes.map(classRep => new WrappingClassfileEntry(classRep))
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
      case (wrappedPackage, pkgName) => 
        wrappedPackage.packages.find(pkg => pkg.name == pkgName).get
    }
    wrappedPackage
  }

  def findClassFile(name: String): Option[AbstractFile] = wrappedClasspath.findClassFile(name)

  protected class WrappingPackageEntry(
      val name: String,
      wrappedPackage: ClassPath[AbstractFile]) extends PackageEntry
  protected class WrappingClassfileEntry(wrappedClassfileRep: ClassPath[AbstractFile]#AnyClassRep)
    extends ClassfileEntry {
    assert(wrappedClassfileRep.binary.isDefined)
    def name = wrappedClassfileRep.name
    def file: AbstractFile = wrappedClassfileRep.binary.get
  }
}
