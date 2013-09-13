package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassFileLookup

/**
 * An interface for a flat classpath.
 *
 * We call this variant of a classpath representation flat because you can
 * query the whole classpath using just single instance implementing this interface.
 *
 * This is an alternative design compared to scala.tools.nsc.util.ClassPath
 */
trait FlatClassPath extends ClassFileLookup {
  /** Empty string represents root package */
  def packages(inPackage: String): Seq[PackageEntry]
  def classes(inPackage: String): Seq[ClassFileEntry]
  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry])
}

object FlatClassPath {
  val RootPackage = ""
}

sealed trait ClassPathEntry {
    def name: String
  }
trait ClassFileEntry extends ClassPathEntry {
  def file: AbstractFile
}
trait PackageEntry extends ClassPathEntry

case class PackageEntryImpl(name: String) extends PackageEntry
