package xsbt
import java.io.File
import java.nio.file.Paths
import java.util.zip.ZipFile

class STJ(outputDirs: Iterable[File]) {
  type JaredClass = String
  type RelClass = String

  def init(jar: File, cls: RelClass): JaredClass = {
    // This identifier will be stored as a java.io.File. Its constructor will normalize slashes
    // which means that the identifier to be consistent should at all points have consistent
    // slashes for safe comparisons, especially in sets or maps.
    val relClass = if (File.separatorChar == '/') cls else cls.replace(File.separatorChar, '/')
    s"$jar!$relClass"
  }

  def init(cls: RelClass): JaredClass = {
    init(outputJar.get, cls)
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
          classes.map(init)
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
