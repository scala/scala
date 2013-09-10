package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import FlatClasspath.RootPackage

/** AbstractFile-backed implementation of a classpath. */
abstract class AbstractFileFlatClasspath(file: AbstractFile) extends FlatClasspath {

  protected class AbstractFilePackageEntry(file: AbstractFile) extends PackageEntry {
    def name = file.name
  }

  protected class AbstractFileClassfileEntry(val file: AbstractFile) extends ClassfileEntry {
    def name = file.name
  }

  private def getDirectory(forPackage: String): AbstractFile = {
    if (forPackage == RootPackage) {
      file
    } else {
      val dirName = forPackage.replace('.', '/')
      file.subdirectoryNamed(dirName)
    }
  }

  def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val dirs = dirForPackage.iterator.filter(_.isDirectory)
    val entries = dirs.toSeq  map { file =>
      new AbstractFilePackageEntry(file)
    }
    entries
  }
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val classfiles = dirForPackage.iterator.filterNot(_.isDirectory)
    val entries = classfiles.toSeq map { file =>
      new AbstractFileClassfileEntry(file)
    }
    entries
  }

  private def packageForClassName(className: String): String = {
    val lastIndex = className.lastIndexOf('.')
    if (lastIndex == -1) RootPackage else {
      className.substring(0, lastIndex-1)
    }
  }

  def findClassFile(className: String): Option[AbstractFile] = {
    val lastIndex = className.lastIndexOf('.')
    val (pkg, simpleClassName) = if (lastIndex == -1) (RootPackage, className) else {
      (className.substring(0, lastIndex-1), className.substring(lastIndex+1))
    }
    classes(pkg).find(_.name == simpleClassName).map(_.file)
  }
}
