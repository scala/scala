package xsbt

import java.io.File
import java.util.zip.ZipFile

/** STJ stands for Straight to Jar compilation.
 *
 *  This is a utility class that provides a set of functions that
 *  are used to implement this feature.
 *
 *  [[sbt.internal.inc.STJ]] is an object that has similar purpose and
 *  duplicates some of the code, as it is difficult to share it.
 */
final class STJ(outputDirs: Iterable[File]) {
  type JaredClass = String
  type RelClass = String

  /** Creates an identifier for a class located inside a jar.
    * For plain class files it is enough to simply use the path.
    * A class in jar `JaredClass` is identified as a path to jar
    * and path to the class within that jar. Those two values
    * are held in one string separated by `!`. Slashes in both
    * paths are consistent with `File.separatorChar` as the actual
    * string is usually kept in `File` object.
    *
    * As an example given a jar file "C:\develop\zinc\target\output.jar"
    * and relative path to the class "sbt/internal/inc/Compile.class"
    * The resulting identifier would be:
    * "C:\develop\zinc\target\output.jar!sbt\internal\inc\Compile.class"
    *
    *  @param jar jar file that contains the class
    *  @param cls relative path to the class within the jar
    *  @return identifier/path to a class in jar.
    */
  def jaredClass(jar: File, cls: RelClass): JaredClass = {
    // This identifier will be stored as a java.io.File. Its constructor will normalize slashes
    // which means that the identifier to be consistent should at all points have consistent
    // slashes for safe comparisons, especially in sets or maps.
    val relClass = if (File.separatorChar == '/') cls else cls.replace('/', File.separatorChar)
    s"$jar!$relClass"
  }

  def jaredClass(cls: RelClass): JaredClass = {
    jaredClass(outputJar.get, cls)
  }

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

  val outputJar: Option[File] = {
    outputDirs match {
      case Seq(file) if file.getName.endsWith(".jar") => Some(file)
      case _                                          => None
    }
  }

  val enabled: Boolean = outputJar.isDefined

  class PrevJarCache(rawClasspath: String) extends scala.collection.generic.Clearable {
    private var cache: Set[JaredClass] = _

    private lazy val prevJar = {
      val classpath = rawClasspath.split(File.pathSeparator)
      findPrevJar(classpath)
    }

    def contains(jaredClass: JaredClass): Boolean = {
      if (cache == null) {
        cache = loadEntriesFromPrevJar()
      }
      cache.contains(jaredClass)
    }

    def clear(): Unit = cache = null

    private def loadEntriesFromPrevJar(): Set[JaredClass] = {
      prevJar
        .filter(_.exists())
        .fold(Set.empty[JaredClass]) { prevJar =>
          val classes = listFiles(prevJar)
          classes.map(jaredClass)
        }
    }
  }

  private def findPrevJar(classpath: Seq[String]): Option[File] = {
    classpath.headOption.map(new File(_)).filter { path =>
      val fileName = path.getName
      fileName.startsWith(prevJarPrefix) && fileName.endsWith(".jar")
    }
  }

  private val prevJarPrefix: String = "prev-jar"

}
