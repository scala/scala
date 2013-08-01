package scala.tools.nsc.classpath

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
  def packages(inPackage: String): Seq[String]
  def classes(inPackage: String): Seq[String]
  def loadClassfile(classfile: String): Array[Byte]
}
