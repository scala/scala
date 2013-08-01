package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile

/**
 * An interface for a flat classpath.
 *
 * We call this variant of a classpath representation flat because you can
 * query the whole classpath using just single instance implementing this interface.
 *
 * This is an alternative design compared to scala.tools.nsc.util.ClassPath
 */
trait FlatClasspath {
  /** Empty string represents root package */
  def packages(inPackage: String): Seq[PackageEntry]
  def classes(inPackage: String): Seq[ClassfileEntry]
  //def loadClassfile(ClassfileEntry: String): Array[Byte]
  
  sealed trait ClasspathEntry {
    def name: String
  }
  trait ClassfileEntry extends ClasspathEntry {
    def file: AbstractFile
  }
  trait PackageEntry extends ClasspathEntry
}

object FlatClasspath {
  val RootPackage = ""
}
