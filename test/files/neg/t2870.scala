class Jars(jar: Jar)

object Jars {
  import scala.util.Properties.javaClassPath

  val scala = fromClasspathString(javaClassPath)

  def fromClasspathString(s: String): Jars = null
}
