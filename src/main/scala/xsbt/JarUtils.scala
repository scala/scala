package xsbt

import java.io.File
import java.util.zip.ZipFile

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
   * Lists regular files (not directories) inside the given jar.
   *
   * @param jar the file to list jars from
   * @return list of paths to files in jar
   */
  def listFiles(jar: File): Set[RelClass] = {
    import scala.collection.JavaConverters._
    // ZipFile is slightly slower than IndexBasedZipFsOps but it is quite difficult to use reuse
    // IndexBasedZipFsOps in compiler bridge.
    val zip = new ZipFile(jar)
    try {
      zip.entries().asScala.filterNot(_.isDirectory).map(_.getName).toSet
    } finally {
      zip.close()
    }
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

  /**
   * Class that holds cached list of paths located within previous jar for quick lookup.
   * @see sbt.internal.inc.JarUtils#withPreviousJar for details on what previous jar is
   */
  class PrevJarCache(prevJar: Option[File]) extends scala.collection.generic.Clearable {
    private var cache: Set[ClassInJar] = _

    def contains(classInJar: ClassInJar): Boolean = {
      if (cache == null) {
        cache = loadEntriesFromPrevJar()
      }
      cache.contains(classInJar)
    }

    def clear(): Unit = cache = null

    private def loadEntriesFromPrevJar(): Set[ClassInJar] = {
      prevJar
        .filter(_.exists())
        .fold(Set.empty[ClassInJar]) { prevJar =>
          val classes = listFiles(prevJar)
          classes.map(ClassInJar)
        }
    }
  }

}
