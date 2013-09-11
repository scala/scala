package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import FlatClasspath.RootPackage
import scala.reflect.io.Path

/** AbstractFile-backed implementation of a classpath. */
abstract class AbstractFileFlatClasspath(file: AbstractFile) extends FlatClasspath {
  import AbstractFileFlatClasspath._
  private def getDirectory(forPackage: String): Option[AbstractFile] = {
    if (forPackage == RootPackage) {
      Some(file)
    } else {
      val dirName = forPackage.replace('.', '/')
      // lookupName might return null but Option.apply will turn it into None
      Option(file.lookupPathUnchecked(dirName, true))
    }
  }
  
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')

  def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val allEntriesInPackage = dirForPackage.toList.flatMap(_.iterator)
    val dirs = allEntriesInPackage.filter(file => file.isDirectory && validPackage(file.name))
    val prefix = if (inPackage == RootPackage) "" else inPackage + "."
    val entries = dirs.toList  map { file =>
      PackageEntryImpl(prefix + file.name)
    }
    entries
  }
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val classfiles = dirForPackage.toList.flatMap(_.iterator.filter(file => !file.isDirectory && file.hasExtension("class")))
    val entries = classfiles.toList map { file =>
      ClassfileEntryImpl(file)
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

object AbstractFileFlatClasspath {
  private case class PackageEntryImpl(name: String) extends PackageEntry
  private case class ClassfileEntryImpl(file: AbstractFile) extends ClassfileEntry {
    def name = {
      val className = Path(file.path).stripExtension
      className
    }
  }
}
