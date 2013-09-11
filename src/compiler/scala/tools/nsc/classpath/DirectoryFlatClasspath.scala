package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import java.io.File
import java.io.FileFilter

case class DirectoryFlatClasspath(dir: File) extends FlatClasspath {
  import FlatClasspath.RootPackage
  import DirectoryFlatClasspath.ClassfileEntryImpl
  assert(dir != null)
  
  private def getDirectory(forPackage: String): Option[File] = {
    if (forPackage == RootPackage) {
      Some(dir)
    } else {
      val packageDirName = forPackage.replace('.', '/')
      val packageDir = new File(dir, packageDirName)
      if (packageDir.exists && packageDir.isDirectory) {
        Some(packageDir)
      } else None
    }
  }
  
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')
  
  private object packageDirectoryFileFilter extends FileFilter {
    def accept(pathname: File): Boolean = pathname.isDirectory && validPackage(pathname.getName)
  }

  def packages(inPackage: String): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[File] = dirForPackage match {
      case None => Array.empty
      case Some(dir) => dir.listFiles(packageDirectoryFileFilter)
    }
    val prefix = if (inPackage == RootPackage) "" else inPackage + "."
    val entries = nestedDirs map { file =>
      PackageEntryImpl(prefix + file.getName)
    }
    entries
  }
  
  private object classFileFileFilter extends FileFilter {
    def accept(pathname: File): Boolean = pathname.isFile && pathname.getName.endsWith(".class")
  }
  
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val classfiles: Array[File] = dirForPackage match {
      case None => Array.empty
      case Some(dir) => dir.listFiles(classFileFileFilter)
    }
    val entries = classfiles map { file =>
      val wrappedFile = new scala.reflect.io.File(file)
      ClassfileEntryImpl(AbstractFile.getFile(wrappedFile))
    }
    entries
  }
  
  def findClassFile(className: String): Option[AbstractFile] = {
    val lastIndex = className.lastIndexOf('.')
    val (pkg, simpleClassName) = if (lastIndex == -1) (RootPackage, className) else {
      (className.substring(0, lastIndex-1), className.substring(lastIndex+1))
    }
    val classfile = new File(dir, className + ".class")
    if (classfile.exists) {
      val wrappedClassFile = new scala.reflect.io.File(classfile)
      val abstractClassFile = AbstractFile.getFile(wrappedClassFile) 
      Some(abstractClassFile)
    } else None
  }
}

object DirectoryFlatClasspath {
  private case class ClassfileEntryImpl(file: AbstractFile) extends ClassfileEntry {
    def name = {
      def stripClassExtension(s: String): String = s.substring(0, s.length-6) // ".class".length == 6
      val className = stripClassExtension(file.name)
      className
    }
  }
}
