package xsbt

import java.io.File

/**
 * This is a utility class that provides a set of functions that
 * are used to implement straight to jar compilation.
 *
 * [[sbt.internal.inc.JarUtils]] is an object that has similar purpose and
 * duplicates some of the code, as it is difficult to share it.
 */
final class JarUtils(outputDirs: Iterable[File]) {
  type ClassInJar = String
  type RelClass = String

  /**
   * Creates an identifier for a class located inside a jar.
   * Mimics the behavior of [[sbt.internal.inc.JarUtils.ClassInJar]].
   */
  def ClassInJar(jar: File, cls: RelClass): ClassInJar = {
    val relClass = if (File.separatorChar == '/') cls else cls.replace('/', File.separatorChar)
    s"$jar!$relClass"
  }

  /** Creates an identifier for a class located inside the current output jar. */
  def ClassInJar(cls: RelClass): ClassInJar = {
    ClassInJar(outputJar.get, cls)
  }

  /**
   * The jar file that is used as output for classes. If the output is
   * not set to a single .jar file, value of this field is [[None]].
   */
  val outputJar: Option[File] = {
    outputDirs match {
      case Seq(file) if file.getName.endsWith(".jar") => Some(file)
      case _                                          => None
    }
  }

  /**
   * Informs if the Straight to Jar compilation feature is enabled,
   * i.e. if the output is set to a jar file.
   */
  val isCompilingToJar: Boolean = outputJar.isDefined

}
